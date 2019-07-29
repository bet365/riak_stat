%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(exoskele_data).

-include("exoskeleskin.hrl").

%% API
-export([
  format_time/1,
  utc_milliseconds_time/0,
  exo_timestamp/0,
  example/0,
  parse_data/1,
  get_port/1,
  get_type/1,
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
  exometer_util:timestamp().

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
    NumList = re:split(S, "\\.", [{return, list}]),
    NewIP =
        case list_to_tuple(NumList) of
            IP when length(NumList) < 5 ->
                IP;
            {error, _Reason} -> Sip;
            _Else -> Sip
        end,
    sanitise_data_(Rest, Protocol, Port, Instance, NewIP);
sanitise_data_([], Protocol, Port, Instance, Sip) ->
  {Protocol, Port, Instance, Sip}.






example() ->
  List = re:split([<<"protocol=udp port=8080 instance=hellothere sip=127.0.0.1">>], "\\s"),
%%  {{Protocol, Port, Instance, Sip},Extra}=
  lists:map(fun
                (<<"protocol=", P/binary>>) ->
                  {protocol, P};
                (<<"port=", P/binary>>) ->
                  {port,P};
                (<<"instance=", I/binary>>) ->
                  {instance,I};
                (<<"sip=", S/binary>>) ->
                  {servr_ip,S}
            end, List).



parse_data([]) ->
  0;
parse_data(Data) when is_list(Data)->
  lists:map(fun(P) ->
    parse_data_(P)
            end, Data);
parse_data(Data) when is_integer(Data) ->
  Data.

parse_data_(Data) when is_binary(Data) ->
  try binary_to_integer(Data)
  catch error:_ -> parse_data_(binary_to_list(Data))
  end;
parse_data_(Data) when is_list(Data) ->
  try list_to_integer(Data)
  catch error:_ -> lager:error("Port is not an integer~n")
  end.


get_port(Port) when is_list(Port) == false andalso is_binary(Port) ->
  get_port([Port]);
get_port([<<"port=", P/binary>> | Rest]) ->
  case P of
    Po when is_binary(Po) ->
      try binary_to_integer(Po)
      catch error: _ -> Rest
      end;
    _ ->
      Rest
  end.


get_type(Type) when is_list(Type) == false andalso is_binary(Type) ->
  get_type([Type]);
get_type([<<"type=", T/binary>> | Rest]) ->
  case T of
    <<"udp">> -> udp;
    <<"http">> -> http;
    _ -> Rest
  end.
