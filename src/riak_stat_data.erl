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
-export([sanitise_data/1, sanitise_func/1,
  find_entries/2]).

-define(PFX, riak_stat_admin:prefix()).

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


-spec(find_entries(Arg :: term()| list(), ToStatus :: atom()) ->
ok | term() | {error, Reason :: term()}).
%% @doc
%% pulls the information of a stat out of exometer
%% @end
find_entries(Arg, ToStatus) ->
%%  lager:error("Arg: ~p ToStatus: ~p~n", [Arg, ToStatus]),
  lists:map(
    fun(A) ->
      {S, Type, Status, DPs} = type_status_and_dps(A, ToStatus),
      case S of
        "[" ++ _ ->
          {find_entries_1(S, Type, Status), DPs};
        _ ->
          case legacy_search(S, Type, Status) of
            false ->
              {find_entries_1(S, Type, Status), DPs};
            Found ->
              {Found, DPs}
          end
      end
    end, Arg).

type_status_and_dps(S, ToStatus) ->
%%  lager:error("S: ~p ToStatus: ~p~n", [S, ToStatus]),
%%  [S1|Rest] = re:split(S, "/", [{return, list}]),
  [S1|Rest] = re:split(S, "/"),
%%  lager:error("S1: ~p Rest: ~p~n", [S1, Rest]),
  {Type, Status, DPs} = type_status_and_dps(Rest, '_', ToStatus, default),
%%  lager:error("Type: ~p Status: ~p DPs: ~p~n", [Type, Status, DPs]),
  {S1, Type, Status, DPs}.

type_status_and_dps([<<"type=", T/binary>>|Rest], _Type, ToStatus, DPs) ->
  NewType = case T of
              <<"*">> -> '_';
              _ ->
                try binary_to_existing_atom(T, latin1)
                catch error:_ -> T
                end
            end,
  type_status_and_dps(Rest, NewType, ToStatus, DPs);
type_status_and_dps([<<"status=", St/binary>>|Rest], Type, _Status, DPs) ->
  NewStatus = case St of
                <<"enabled">>  -> enabled;
                <<"disabled">> -> disabled;
                <<"*">>        -> '_'
              end,
  type_status_and_dps(Rest, Type, NewStatus, DPs);
type_status_and_dps([DPsBin|Rest], Type, Status, DPs) ->
  NewDPs = merge([binary_to_existing_atom(D,latin1)
    || D <- re:split(DPsBin, ",")], DPs),
  type_status_and_dps(Rest, Type, Status, NewDPs);
type_status_and_dps([], Type, Status, DPs) ->
  {Type, Status, DPs}.

merge([_|_] = DPs, default) ->
  DPs;
merge([H|T], DPs) ->
  case lists:member(H, DPs) of
    true  -> merge(T, DPs);
    false -> merge(T, DPs ++ [H])
  end;
merge([], DPs) ->
  DPs.

find_entries_1(S, Type, Status) ->
  Patterns = lists:flatten([parse_stat_entry(S, Type, Status)]),
  riak_stat_mngr:select_stat(Patterns).

parse_stat_entry([], Type, Status) -> % change this for a more generic version
  {{[?PFX] ++ '_', Type, '_'}, [{'=:=','$status',Status}], ['$_']};
parse_stat_entry("*", Type, Status) ->
  parse_stat_entry([], Type, Status);
parse_stat_entry("[" ++ _ = Expr, _Type, _Status) ->
  case erl_scan:string(ensure_trailing_dot(Expr)) of
    {ok, Toks, _} ->
      case erl_parse:parse_exprs(Toks) of
        {ok, [Abst]} ->
          partial_eval(Abst);
        Error ->
          io:fwrite("(Parse error for ~p: ~p~n", [Expr, Error]),
          []
      end;
    ScanErr ->
      io:fwrite("(Scan error for ~p: ~p~n", [Expr, ScanErr]),
      []
  end;
parse_stat_entry(Str, Type, Status) when Status==enabled; Status==disabled ->
  Parts = re:split(Str, "\\.", [{return,list}]),
  Heads = replace_parts(Parts),
  [{{H,Type,Status}, [], ['$_']} || H <- Heads];
parse_stat_entry(Str, Type, '_') ->
  Parts = re:split(Str, "\\.", [{return,list}]),
  Heads = replace_parts(Parts),
  [{{H,Type,'_'}, [], ['$_']} || H <- Heads];
parse_stat_entry(_, _, Status) ->
  io:fwrite("(Illegal status: ~p~n", [Status]).

ensure_trailing_dot(Str) ->
  case lists:reverse(Str) of
    "." ++ _ ->
      Str;
    _ ->
      Str ++ "."
  end.

partial_eval({cons,_,H,T}) ->
  [partial_eval(H) | partial_eval(T)];
partial_eval({tuple,_,Elems}) ->
  list_to_tuple([partial_eval(E) || E <- Elems]);
partial_eval({op,_,'++',L1,L2}) ->
  partial_eval(L1) ++ partial_eval(L2);
partial_eval(X) ->
  erl_parse:normalise(X).

replace_parts(Parts) ->
  case split("**", Parts) of
    {_, []} ->
      [replace_parts_1(Parts)];
    {Before, After} ->
      Head = replace_parts_1(Before),
      Tail = replace_parts_1(After),
      [Head ++ Pad ++ Tail || Pad <- pads()]
  end.

split(X, L) ->
  split(L, X, []).

split([H|T], H, Acc) ->
  {lists:reverse(Acc), T};
split([H|T], X, Acc) ->
  split(T, X, [H|Acc]);
split([], _, Acc) ->
  {lists:reverse(Acc), []}.

replace_parts_1([H|T]) ->
  R = replace_part(H),
  case T of
    ["**"] -> [R] ++ '_';
    _ -> [R|replace_parts_1(T)]
  end;
replace_parts_1([]) ->
  [].

replace_part(H) ->
  case H of
    "*" -> '_';
    "'" ++ _ ->
      case erl_scan:string(H) of
        {ok, [{atom, _, A}], _} ->
          A;
        Error ->
          error(Error)
      end;
    [C|_] when C >= $0, C =< $9 ->
      try list_to_integer(H)
      catch
        error:_ -> list_to_atom(H)
      end;
    _ -> list_to_atom(H)
  end.

pads() ->
  [['_'],
    ['_','_'],
    ['_','_','_'],
    ['_','_','_','_'],
    ['_','_','_','_','_'],
    ['_','_','_','_','_','_'],
    ['_','_','_','_','_','_','_'],
    ['_','_','_','_','_','_','_','_'],
    ['_','_','_','_','_','_','_','_','_'],
    ['_','_','_','_','_','_','_','_','_','_'],
    ['_','_','_','_','_','_','_','_','_','_','_'],
    ['_','_','_','_','_','_','_','_','_','_','_','_'],
    ['_','_','_','_','_','_','_','_','_','_','_','_','_'],
    ['_','_','_','_','_','_','_','_','_','_','_','_','_','_'],
    ['_','_','_','_','_','_','_','_','_','_','_','_','_','_','_'],
    ['_','_','_','_','_','_','_','_','_','_','_','_','_','_','_','_']].

legacy_search(S, Type, Status) ->
  case re:run(S, "\\.", []) of
    {match,_} ->
      false;
    nomatch ->
      Re = <<"^", (make_re(S))/binary, "$">>,
      [{S, legacy_search_1(Re, Type, Status)}]
  end.

make_re(S) ->
  repl(split_pattern(S, [])).

legacy_search_1(N, Type, Status) ->
  Found = aliases(regexp_foldr, [N]),
  lists:foldr(
    fun({Entry, DPs}, Acc) ->
      case match_type(Entry, Type) of
        true ->
          DPnames = [D || {D,_} <- DPs],
          case get_datapoint(Entry, DPnames) of
            {ok, Values} when is_list(Values) ->
              [{Entry, zip_values(Values, DPs)} | Acc];
            {ok, disabled} when Status=='_';
              Status==disabled ->
              [{Entry, zip_disabled(DPs)} | Acc];
            _ ->
              [{Entry, [{D,undefined} || D <- DPnames]}|Acc]
          end;
        false ->
          Acc
      end
    end, [], orddict:to_list(Found)).

match_type(_, '_') ->
  true;
match_type(Name, T) ->
  T == get_info(Name, type).

get_info(Name, Info) -> % Todo: rremove this
  case riak_stat_mngr:get_info(Name, Info) of
    undefined ->
      [];
    Other ->
      Other
  end.

aliases(Type, Entries) -> % todo: is this necessary??
  riak_stat_mngr:aliases(Type, Entries).

get_datapoint(Name, DP) -> % todo: maybe keep this
  riak_stat_mngr:get_datapoint(Name, DP).


zip_values([{D,V}|T], DPs) ->
  {_,N} = lists:keyfind(D, 1, DPs),
  [{D,V,N}|zip_values(T, DPs)];
zip_values([], _) ->
  [].

zip_disabled(DPs) ->
  [{D,disabled,N} || {D,N} <- DPs].

repl([single|T]) ->
  <<"[^_]*", (repl(T))/binary>>;
repl([double|T]) ->
  <<".*", (repl(T))/binary>>;
repl([H|T]) ->
  <<H/binary, (repl(T))/binary>>;
repl([]) ->
  <<>>.

split_pattern(<<>>, Acc) ->
  lists:reverse(Acc);
split_pattern(<<"**", T/binary>>, Acc) ->
  split_pattern(T, [double|Acc]);
split_pattern(<<"*", T/binary>>, Acc) ->
  split_pattern(T, [single|Acc]);
split_pattern(B, Acc) ->
  case binary:match(B, <<"*">>) of
    {Pos,_} ->
      <<Bef:Pos/binary, Rest/binary>> = B,
      split_pattern(Rest, [Bef|Acc]);
    nomatch ->
      lists:reverse([B|Acc])
  end.


