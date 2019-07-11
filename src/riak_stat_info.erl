%%%-------------------------------------------------------------------
%%% @doc
%%% I/O functions for printing data, or for pulling our additional
%%% information needed for specific modules
%%% @end
%%%-------------------------------------------------------------------
-module(riak_stat_info).

-include("riak_stat.hrl").

%% API
-export([
  print/1,
  print/2,
  pick_info_attrs/1]).

-spec(print(data()) -> print()).
%% @doc print argument with all its info @end
print(Arg) ->
  Arg1 = re:split(Arg, "\\s", [{return, list}]),
  {Attr, RestArg} = pick_info_attrs(Arg1),
  print(RestArg, Attr).

-spec(pick_info_attrs(data()) -> value()).
%% @doc get list of attrs to print @end
pick_info_attrs(Arg) ->
  case lists:foldr(
    fun("-name", {As, Ps}) -> {[name | As], Ps};
      ("-type", {As, Ps}) -> {[type | As], Ps};
      ("-module", {As, Ps}) -> {[module | As], Ps};
      ("-value", {As, Ps}) -> {[value | As], Ps};
      ("-cache", {As, Ps}) -> {[cache | As], Ps};
      ("-status", {As, Ps}) -> {[status | As], Ps};
      ("-timestamp", {As, Ps}) -> {[timestamp | As], Ps};
      ("-options", {As, Ps}) -> {[options | As], Ps};
      (P, {As, Ps}) -> {As, [P | Ps]}
    end, {[], []}, Arg) of
    {[], Rest} ->
      {[name, type, module, value, cache, status, timestamp, options], Rest};
    Other ->
      Other
  end.

-spec(print(data(), attr() | term()) -> print()).
%% @doc
%% Print stats is generic, and used by both stat show and stat info,
%% Stat info includes all the attributes that will be printed whereas stat show
%% will pass in an empty list into the Attributes field.
%% @end
print([], _) ->
  io:fwrite("No matching stats~n");
print({[{LP, []}], _}, _) ->
  io:fwrite("== ~s (Legacy pattern): No matching stats ==~n", [LP]);
print({[{LP, Matches}], _}, []) ->
  io:fwrite("== ~s (Legacy pattern): ==~n", [LP]),
  [[io:fwrite("~p: ~p (~p/~p)~n", [N, V, E, DP])
    || {DP, V, N} <- DPs] || {E, DPs} <- Matches];
print({[{LP, Matches}], _}, Attrs) ->
  io:fwrite("== ~s (Legacy pattern): ==~n", [LP]),
  lists:foreach(
    fun({N, _}) ->
      print_info_1(N, Attrs)
    end, Matches);
print({[], _}, _) ->
  io_lib:fwrite("No matching stats~n", []);
print({Entries, DPs}, []) ->
  [io:fwrite("~p: ~p~n", [E, get_value(E, Status, DPs)])
    || {E, _, Status} <- Entries];
print({Entries, _}, Attrs) ->
  lists:foreach(
    fun({N, _, _}) ->
      print_info_1(N, Attrs)
    end, Entries);
%%print_stats([{Entries, DPS}], Att) ->
%%  print_stats({Entries, DPS}, Att);
print(Data, Att) ->
  print({[{Data, [], []}], []}, Att).

get_value(_, disabled, _) ->
  disabled;
get_value(E, _Status, DPs) ->
  case get_datapoint(E, DPs) of
    {ok, V} -> V;
    {error, _} -> unavailable
  end.

get_datapoint(Name, DP) ->
  riak_stat_coordinator:get_datapoint(Name, DP).

% used to print the entire stat information
print_info_1(N, [A | Attrs]) ->
  Hdr = lists:flatten(io_lib:fwrite("~p: ", [N])),
  Pad = lists:duplicate(length(Hdr), $\s),
  Info = get_info(core, N),
  Status = proplists:get_value(status, Info, enabled),
  Body = [io_lib:fwrite("~w = ~p~n", [A, proplists:get_value(A, Info)])
    | lists:map(fun(value) ->
      io_lib:fwrite(Pad ++ "~w = ~p~n",
        [value, get_value(N, Status, default)]);
      (Ax) ->
        io_lib:fwrite(Pad ++ "~w = ~p~n",
          [Ax, proplists:get_value(Ax, Info)])
                end, Attrs)],
  io:put_chars([Hdr, Body]).


get_info(Name, Info) ->
  case riak_stat_coordinator:get_info(Name, Info) of
    undefined ->
      [];
    Other ->
      Other
  end.