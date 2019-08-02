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
-spec(sanitise_data(arg()) -> sanitised_data()).
%% @doc
%% Sanitise the data coming in, into the necessary arguments for setting
%% up an endpoint
%% @end
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
  sanitise_data_(Arg, ?MONITOR_STATS_PORT, ?INSTANCE, ?MONITOR_SERVER).
sanitise_data_([<<"port=", Po/binary>> | Rest], Port, Instance, Sip) ->
  NewPort =
    case binary_to_integer(Po) of
      {error, _reason} -> Port;
      Int -> Int
    end,
  sanitise_data_(Rest, NewPort, Instance, Sip);
sanitise_data_([<<"instance=", I/binary>> | Rest], Port, _Instance, Sip) ->
  NewInstance = binary_to_list(I),
  sanitise_data_(Rest, Port, NewInstance, Sip);
sanitise_data_([<<"sip=", S/binary>> | Rest], Port, Instance, _Sip) ->
  NewIP = re:split(S, "\\s", [{return, list}]),
  sanitise_data_(Rest, Port, Instance, NewIP);
sanitise_data_([], Port, Instance, Sip) ->
  {Port, Instance, Sip}.



%%--------------------------------------------------------------------
-spec(format_time(erlang:timestamp()) -> string()).
%% @doc
%% For a given timestamp, returns the string representation in the following
%% format, YYYY.MM.ddTHH:mm:ss.SSSZ the time is in UTC.
%% @end
format_time({ _, _, MicroSeconds} = Now) ->
    {{Year, Month, Day},{ Hour, Min, Sec}} = calendar:now_to_universal_time(Now),
    lists:flatten(io_lib:format("~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0w.~.3.0wZ",
        [Year, Month, Day, Hour, Min, Sec, MicroSeconds div ?MILLISECONDS_MICROSECONDS_DIVISOR])).

%%--------------------------------------------------------------------
-spec(utc_milliseconds_time() -> term()).
%% @doc
%% Gets the utc time in milliseconds.
%% @end
utc_milliseconds_time() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * ?MEGASECS_MILLISECOND_DIVISOR) + (Sec * ?SECS_MILLISECOND_DIVISOR) + (Micro div ?MILLISECONDS_MICROSECONDS_DIVISOR).

%%--------------------------------------------------------------------
-spec(exo_timestamp() -> term()).
%% @doc
%% timestamp format used with exometer_histogram and exometer_slide
%% @end
exo_timestamp() ->
    riak_stat:timestamp().




%%%===================================================================
%%% Extra API
%%%===================================================================

%%%%--------------------------------------------------------------------
%%%% @doc
%%%% Calculates the execution time of the fun and stores it against the
%%%% metric name
%%%% @end
%%%%--------------------------------------------------------------------
%%-spec(execution_time(metric(), fun()) -> any()).
%%execution_time(Metric, Fun) ->
%%  StartTimestamp = utc_milliseconds_time(),
%%
%%  Response = Fun(),
%%
%%  EndTimestamp = utc_milliseconds_time(),
%%  update_histogram(Metric, EndTimestamp - StartTimestamp),
%%  Response.
