%%%-------------------------------------------------------------------
%%% @author savannahallsop
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2019 14:50
%%%-------------------------------------------------------------------
-module(riak_stat_data).
-author("savannahallsop").

%% API
-export([sanitise_data/1, sanitise_func/1]).



%% sanitise_data(Arg) ->
%%
%% behave in a similar way to the find_entries, where it will parse the
%% data into a list, the way it is stored in metadata and exometer.

%% @doc
%% sanitises the data from ["riak.riak_kv.**"] into [riak,riak_kv]
%% @end
sanitise_data({Arg, Status}) ->
  Arg1 = sanitise_data(Arg),
  {Arg1, Status};
sanitise_data(Arg) ->
  lists:map(fun(A) ->
  parse_stat_entry(A)
            end, Arg).

sanitise_func(Func) when is_atom(Func) ->
  sanitise_func(atom_to_list(Func));
sanitise_func(Func) when is_list(Func) ->
  list_to_atom(lists:flatten(func_sanitiser(Func))).

func_sanitiser(Func) ->
  case lists:foldl(fun(RealFuns, TrueFun) ->
    case string:equal(RealFuns, Func) of
      true ->
        [Func | TrueFun];
      false ->
        TrueFun
    end
                   end, [], functions()) of
    [] ->
      ["no_function_found"];
    Fun ->
      Fun
  end.

functions() ->
  [ % profiles
    "load_profile",
    "add_profile",
    "add_profile_stat",
    "remove_profile",
    "remove_profile_stat",
    "reset_profiles",
    "check_profile_stat",
    "change_profile_stat",
    % console
    "stat_show",
    "stat_info",
    "stat_enable",
    "stat_disable",
    "stat_show_0",
    "stat_disable_0",
    "stat_reset",
    "stat_disabled",
    % admin
    "register",
    "update",
    "read"
  ].

prefix() ->
  app_helper:get_env(riak_stat, prefix, riak).
%%
%%sani_data_for_patterns(Stat, Type, Status) ->
%%ok.
%%
%%%%  case parse_stat_entry(S) of
%%%%     [] ->
%%%%       [];
%%%%    [P|'_'] when P == Prefix ->
%%%%     [{{[P|'_'], Type, '_'}, [{'=:=', '$status', Status}], ['$_']}];
%%%%    Stats when is_list(Stats) ->
%%%%
%%%%
%%%%  end end, Stat.
%%
%%parse_stat_entry([]) ->
%%  [prefix()] ++ '_';
%%parse_stat_entry("*") ->
%%  parse_stat_entry([]);
%%parse_stat_entry("[" ++ _ =Expr) ->
%%  case erl_scan:string(ensure_trailing_dot(Expr)) of
%%    {ok, Toks, _} ->
%%      case erl_parse:parse_exprs(Toks) of
%%        {ok, [Abst]} ->
%%          partial_eval(Abst);
%%        Error ->
%%          io:fwrite("(Parse error for ~p: ~p~n", [Expr, Error]),
%%          []
%%      end;
%%    ScanErr ->
%%      io:fwrite("(Scan error for ~p: ~p~n", [Expr, ScanErr]),
%%      []
%%  end;
%%parse_stat_entry(Str) ->
%%  Parts = re:split(Str, "\\.", [{return,list}]),
%%  replace_parts(Parts). % list of stats names to use in exometer:select
%%
%%ensure_trailing_dot(Str) ->
%%  case lists:reverse(Str) of
%%    "." ++ _ ->
%%      Str;
%%    _ ->
%%      Str ++ "."
%%  end.
%%
%%partial_eval({cons,_,H,T}) ->
%%  [partial_eval(H) | partial_eval(T)];
%%partial_eval({tuple,_,Elems}) ->
%%  list_to_tuple([partial_eval(E) || E <- Elems]);
%%partial_eval({op,_,'++',L1,L2}) ->
%%  partial_eval(L1) ++ partial_eval(L2);
%%partial_eval(X) ->
%%  erl_parse:normalise(X).
%%
%%replace_parts(Parts) ->
%%  case split("**", Parts) of
%%    {_, []} ->
%%      [replace_parts_1(Parts)];
%%    {Before, After} ->
%%      Head = replace_parts_1(Before),
%%      Tail = replace_parts_1(After),
%%      [Head ++ Pad ++ Tail || Pad <- pads()]
%%  end.
%%
%%split(X, L) ->
%%  split(L, X, []).
%%
%%split([H|T], H, Acc) ->
%%  {lists:reverse(Acc), T};
%%split([H|T], X, Acc) ->
%%  split(T, X, [H|Acc]);
%%split([], _, Acc) ->
%%  {lists:reverse(Acc), []}.
%%
%%replace_parts_1([H|T]) ->
%%  R = replace_part(H),
%%  case T of
%%    ["**"] -> [R] ++ '_';
%%    _ -> [R|replace_parts_1(T)]
%%  end;
%%replace_parts_1([]) ->
%%  [].
%%
%%replace_part(H) ->
%%  case H of
%%    "*" -> '_';
%%    "'" ++ _ ->
%%      case erl_scan:string(H) of
%%        {ok, [{atom, _, A}], _} ->
%%          A;
%%        Error ->
%%          error(Error)
%%      end;
%%    [C|_] when C >= $0, C =< $9 ->
%%      try list_to_integer(H)
%%      catch
%%        error:_ -> list_to_atom(H)
%%      end;
%%    _ -> list_to_atom(H)
%%  end.
%%
%%pads() ->
%%  [['_'],
%%    ['_','_'],
%%    ['_','_','_'],
%%    ['_','_','_','_'],
%%    ['_','_','_','_','_'],
%%    ['_','_','_','_','_','_'],
%%    ['_','_','_','_','_','_','_'],
%%    ['_','_','_','_','_','_','_','_'],
%%    ['_','_','_','_','_','_','_','_','_'],
%%    ['_','_','_','_','_','_','_','_','_','_'],
%%    ['_','_','_','_','_','_','_','_','_','_','_'],
%%    ['_','_','_','_','_','_','_','_','_','_','_','_'],
%%    ['_','_','_','_','_','_','_','_','_','_','_','_','_'],
%%    ['_','_','_','_','_','_','_','_','_','_','_','_','_','_'],
%%    ['_','_','_','_','_','_','_','_','_','_','_','_','_','_','_'],
%%    ['_','_','_','_','_','_','_','_','_','_','_','_','_','_','_','_']].
%%
%%
%%
%%
%%type_status_and_dps(S, ToStatus) ->
%%  [S1|Rest] = re:split(S,"/"),
%%  {Type, Status, DPs} = type_status_and_dps(Rest, '_', ToStatus ,default),
%%  {S1, Type, Status, DPs}.
%%
%%type_status_and_dps([<<"type=", T/binary>>|Rest], _Type, ToStatus, DPs) ->
%%  NewType = case T of
%%              <<"*">> -> '_';
%%              _ ->
%%                try binary_to_existing_atom(T, latin1)
%%                catch error:_ -> T
%%                end
%%            end,
%%  type_status_and_dps(Rest, NewType, ToStatus, DPs);
%%type_status_and_dps([<<"status=", St/binary>>|Rest], Type, _Status, DPs) ->
%%  NewStatus = case St of
%%                <<"enabled">>  -> enabled;
%%                <<"disabled">> -> disabled;
%%                <<"*">>        -> '_'
%%              end,
%%  type_status_and_dps(Rest, Type, NewStatus, DPs);
%%type_status_and_dps([DPsBin|Rest], Type, Status, DPs) ->
%%  NewDPs = merge([binary_to_existing_atom(D,latin1)
%%    || D <- re:split(DPsBin, ",")], DPs),
%%  type_status_and_dps(Rest, Type, Status, NewDPs);
%%type_status_and_dps([], Type, Status, DPs) ->
%%  {Type, Status, DPs}.

%%merge([_|_] = DPs, default) ->
%%  DPs;
%%merge([H|T], DPs) ->
%%  case lists:member(H, DPs) of
%%    true  -> merge(T, DPs);
%%    false -> merge(T, DPs ++ [H])
%%  end;
%%merge([], DPs) ->
%%  DPs.

