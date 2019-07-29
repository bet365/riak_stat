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
    List = break_up(Arg),
    sanitise_data_(List).

break_up(Arg) ->
   re:split(Arg, "\\s", []).

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
sanitise_data_([<<"sip=", S/binary>> | Rest], Protocol, Port, Instance, Sip) ->
    NewIP = re:split(S, "\\s", [{return, list}]),
    sanitise_data_(Rest, Protocol, Port, Instance, NewIP);
sanitise_data_([], Protocol, Port, Instance, Sip) ->
    {Protocol, Port, Instance, Sip}.