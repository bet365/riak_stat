%%%-------------------------------------------------------------------
%%% @doc
%%% Timestamps, formatting and sanitised data
%%% @end
%%%-------------------------------------------------------------------
-module(exoskele_data).

-include("exoskeleskin.hrl").

%% API
-export([
  format_time/1,
  utc_milliseconds_time/0,
  exo_timestamp/0,
  sanitise_data/1
]).

-define(MEGASECS_MILLISECOND_DIVISOR,1000000000).
-define(SECS_MILLISECOND_DIVISOR,1000).
-define(MILLISECONDS_MICROSECONDS_DIVISOR,1000).

%%--------------------------------------------------------------------
%% @doc
%% For a given timestamp, returns the string representation in the following
%% format, YYYY.MM.ddTHH:mm:ss.SSSZ the time is in UTC.
%% @end
%%--------------------------------------------------------------------
-spec(format_time(erlang:timestamp()) -> string()).
format_time({ _, _, MicroSeconds} = Now) ->
    {{Year, Month, Day},{ Hour, Min, Sec}} = calendar:now_to_universal_time(Now),
    lists:flatten(io_lib:format("~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0w.~.3.0wZ",
        [Year, Month, Day, Hour, Min, Sec, MicroSeconds div ?MILLISECONDS_MICROSECONDS_DIVISOR])).

%%--------------------------------------------------------------------
%% @doc
%% Gets the utc time in milliseconds.
%% @end
%%--------------------------------------------------------------------
-spec(utc_milliseconds_time() -> term()).
utc_milliseconds_time() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * ?MEGASECS_MILLISECOND_DIVISOR) + (Sec * ?SECS_MILLISECOND_DIVISOR) + (Micro div ?MILLISECONDS_MICROSECONDS_DIVISOR).

%%--------------------------------------------------------------------
%% @doc
%% timestamp format used with exometer_histogram and exometer_slide
%% @end
%%--------------------------------------------------------------------
-spec(exo_timestamp() -> term()).
exo_timestamp() ->
    riak_stat:timestamp().

%%--------------------------------------------------------------------
%% @doc
%% Sanitise the data coming in, into the necessary arguments for setting
%% up an endpoint
%% @end
%%--------------------------------------------------------------------
-spec(sanitise_data(term()) -> sanitised_data()).
sanitise_data(Arg) ->
    [Opts | Stats] = break_up(Arg, "/"),
    List = break_up(Opts, "\\s"),
    NewStats =
      case Stats of
        [] -> ['_'];
        Data -> Data
      end,
    {sanitise_data_(List), NewStats}.

break_up(Arg, Str) ->
   re:split(Arg, Str, []).

sanitise_data_(Arg) ->
    sanitise_data_(Arg, undefined, ?MONITOR_STATS_PORT, ?INSTANCE, ?MONITOR_SERVER).
sanitise_data_([<<"protocol=", P/binary>> | Rest], _Protocol, Port, Instance, Sip) ->
    NewProtocol =
        case P of
          <<"udp">> -> P;
          <<"http">> -> P;
          _ -> undefined
        end,
    sanitise_data_(Rest, NewProtocol, Port, Instance, Sip);
sanitise_data_([<<"port=", Po/binary>> | Rest], Protocol, Port, Instance, Sip) ->
    NewPort =
        case binary_to_integer(Po) of
          {error, _reason} -> Port;
          Int -> Int
        end,
    sanitise_data_(Rest, Protocol, NewPort, Instance, Sip);
sanitise_data_([<<"instance=", I/binary>> | Rest], Protocol, Port, _Instance, Sip) ->
    NewInstance = binary_to_list(I),
    sanitise_data_(Rest, Protocol, Port, NewInstance, Sip);
sanitise_data_([<<"sip=", S/binary>> | Rest], Protocol, Port, Instance, _Sip) ->
    NewIP = re:split(S, "\\s", [{return, list}]),
    sanitise_data_(Rest, Protocol, Port, Instance, NewIP);
sanitise_data_([], Protocol, Port, Instance, Sip) ->
    {Protocol, Port, Instance, Sip}.

%% TODO: take info from the do_updates function to add this in
%% like take out the {time_span, ?HISTOGRAM_SPAN} etc.

%%--------------------------------------------------------------------
%% @doc
%% Calculates the execution time of the fun and stores it agaisnt the
%% metric name
%% @end
%%--------------------------------------------------------------------
-spec(execution_time(metric(), fun()) -> any()).
execution_time(Metric, Fun) ->
  StartTimestamp = utc_milliseconds_time(),

  Response = Fun(),

  EndTimestamp = utc_milliseconds_time(),
  update_histogram(Metric, EndTimestamp - StartTimestamp),
  Response.

%% TODO: consolidate the update functions into one (down)

%%--------------------------------------------------------------------
%% @doc
%% Updates a counter statistic by the given value
%% @end
%%--------------------------------------------------------------------
-spec(update_counter(metric(), value()) -> ok).
update_counter(Metric, Value) when is_list(Metric), is_integer(Value) ->
  do_update(get_name(Metric, counter), counter, Value, []).

%%--------------------------------------------------------------------
%% @doc
%% Updates a gauge statistic to the given value, the gauges value is
%% always the most recent provided value.
%% @end
%%--------------------------------------------------------------------
-spec(update_gauge(metric(), value()) -> ok).
update_gauge(Metric, Value) when is_list(Metric), is_integer(Value) ->
  do_update(get_name(Metric, gauge), gauge, Value, []).

%%--------------------------------------------------------------------
%% @doc
%% Updates a spiral statistic, spiral statistics maintain the total sum
%% of all values provided within the timespan, the default is 6 seconds
%% @end
%%--------------------------------------------------------------------
-spec(update_spiral(metric(), value()) -> ok).
update_spiral(Metric, Value) when is_list(Metric), is_integer(Value) ->
  do_update(get_name(Metric, spiral), spiral, Value, [{time_span, ?SPIRAL_TIME_SPAN}]).


%%--------------------------------------------------------------------
%% @doc
%% Stores a given number of updates in a histogram.
%% @end
%%--------------------------------------------------------------------
-spec(update_histogram(metric(), value()) -> ok).
update_histogram(Metric, Value) when is_list(Metric), is_integer(Value) ->
  do_update(get_name(Metric, histogram), histogram, Value, [{time_span, ?HISTOGRAM_TIME_SPAN}]).

%%--------------------------------------------------------------------
%% @doc
%% Get the name of the metric by appended the type to it, this is to
%% distinguish with metrics of the same name.
%% @end
%%--------------------------------------------------------------------
-spec(get_name(metric(), type()) -> metric()).
get_name(Metric, Type) ->
  Metric ++ [Type].

%% TODO: consolidate the below functions into one functioning one

%%--------------------------------------------------------------------
%% @doc
%% update the statistic, checking whether or not it exists. If it does
%% not then create it and and register with the exometer collectd
%%--------------------------------------------------------------------
-spec(do_update(metric(), type(), value(), [{atom(), term()}]) -> ok).
do_update(Metric, Type, Value, Opts) ->
  try

    case exometer:update(Metric, Value) of
      {error, not_found} ->
        ok = case exometer:info(Metric, name) of
               undefined ->
                 lager:info("registering Metric=~p, Type=~p, Options=~p with exometer", [Metric, Type, Opts]),
                 ok = create_statistic(Metric, Type, Opts);
               _ ->
                 ok
             end,
        exometer:update(Metric, Value);
      ok -> ok
    end


  catch Class:Reason ->
    lager:error("Unable to update metric=~p, type=~p, Reason={~p,~p} stacktrace=~p~n",
      [Metric, Type, Class, Reason, erlang:get_stacktrace()])
  end.

-spec(update_or_create(statname(), value(), type(), options()) ->ok).
update_or_create(Name, Val, Type, Opts) ->
  exometer:update_or_create(Name, Val, Type, Opts).