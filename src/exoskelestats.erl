%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(exoskelestats).
-author("savannahallsop").

%% API
-export([
  format_time/1,
  utc_milliseconds_time/0,
  exo_timestamp/0,
  parse_arg/1,
  parse_data/1,
  get_port/1,
  get_type/1
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
-spec(utc_milliseconds_time() -> timestamp()).
utc_milliseconds_time() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * ?MEGASECS_MILLISECOND_DIVISOR) + (Sec * ?SECS_MILLISECOND_DIVISOR) + (Micro div ?MILLISECONDS_MICROSECONDS_DIVISOR).

%%--------------------------------------------------------------------
%% @doc
%% timestamp format used with exometer_histogram and exometer_slide
%% @end
%%--------------------------------------------------------------------
-spec(exo_timestamp() -> timestamp()).
exo_timestamp() ->
  exometer_util:timestamp().




parse_arg(Arg) ->
  parse_arg(Arg, {undefined, 0}).

parse_arg([], {Type, Port}) ->
  {Type, Port};
parse_arg([<<"type=", T/binary>> | Rest], {_Type, Port}) ->
  NewType =
  case T of
    <<"udp">> -> udp;
    <<"http">> -> http
  end,
  parse_arg(Rest, {NewType, Port});
parse_arg([<<"port=", P/binary>> | Rest], {Type, _Port}) ->
  NewPort = parse_data_(P),
  parse_arg(Rest, {Type, NewPort});
parse_arg(Nothing, _Arg) ->
  lager:error("Invalid format: ~p~n", [Nothing]).



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
    Po ->
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
