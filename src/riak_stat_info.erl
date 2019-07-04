%%%-------------------------------------------------------------------
%%% @author savannahallsop
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2019 14:42
%%%-------------------------------------------------------------------
-module(riak_stat_info).
-author("savannahallsop").

%% API
-export([print/1, print/2]).

%%find_entries(Arg, Status) ->
%%  riak_stat_data:find_entries(Arg, Status).

print(Arg) ->
  Arg1 = re:split(Arg, "\\s", [{return, list}]),
  {Attr, RestArg} = pick_info_attrs(Arg1),
  print(RestArg, Attr).

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

-spec(print(Entries :: term(), Attributes :: list() | term()) -> term() | ok).
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
  riak_stat_coordinator:coordinate(get_datapoint,{Name, DP}).

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
  case riak_stat_coordinator:coordinate(get_info,{Name, Info}) of
    undefined ->
      [];
    Other ->
      Other
  end.


%%print_stats0(Stats) ->
%%  lists:foldl(
%%    fun(Stat, Acc) ->
%%      case prin_stat0(Stat) of
%%        {_H, disabled, _} ->
%%          Acc;
%%        {H, Status, _} ->
%%          [{H, Status} | Acc]
%%%%        {_H, _S, []} ->
%%%%          Acc;
%%%%        {_H, _S, _V} ->
%%%%          Acc
%%      end
%%%%      [prin_stat0(Stat) | Acc]
%%    end, [], Stats
%%  ).

%%prin_stat0(Stat) ->
%%  H = lists:flatten(io_lib:fwrite("~p: ", [Stat])),
%%%%  Pad = lists:duplicate(length(H), $\s),
%%  Info = get_info(core, Stat),
%%  Status = io:fwrite("~w = ~p~n", [status, proplists:get_value(status, Info, enabled)]),
%%  Value = io:fwrite("~w = ~p~n", [value, proplists:get_value(value, Info)]),
%%%%  io:put_chars([H, Status, Value]).
%%  {H, Status, Value}.
%%
%%just_print(Stats) ->
%%  io:fwrite("Stats ~p~n~n", [length(Stats)]),
%%  lists:foreach(fun({Stat, _Val}) ->
%%    print(find_entries(Stat, enabled), [value])
%%                end, Stats).
%%just_print(Stat, Status) ->
%%  io:fwrite("~p: ~p~n", [Stat, Status]).


%% print_stats is in this module, anything that needs io:fwrite or a response
%% to a user will function call into this module,

%% holds responses to the client as well

%% TODO: print_stats

