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

%% Sanitising API
-export([parse_info/2]).

%% API
-export([find_entries/2]).

-type data()    :: list() | binary() | atom().
-type statlist():: list().
-type status()  :: atom().
-type reason()  :: any().
-type error()   :: {error, reason()}.

-define(PFX, riak_stat:prefix()).

-spec(parse_info(data(), status()) -> statlist()).
%% @doc
%% The data goes into parse_info and returns as a tuple of the
%% {Statname, Exometer:selectPattern, {Type, Status, DPs}}
%%
%% the statname is the Key that is stored in the metadata and
%% in exometer, most data gets passed through here for the the
%% kv-stores to stay linear.

%% status is useful for find_entries
%% @end
parse_info([], _Status) ->
  no_data;
parse_info(Data, Status) ->
  lists:map(fun(A) ->
    parse_info_(A, Status)
            end, Data).

parse_info_(Data, Status) when is_list(Data) ->
  io:format("Data1: ~p~n", [Data]),
  [R | Est] = re:split(Data, "/"), %% returns R = <<"riak.**">> Est = []
  io:format("R: ~p, Est: ~p~n", [R, Est]),
  Ether = est(Est, '_', Status, default),
%%  {Stats, ExSelectPattern, {Type, Status, Dps}} =
  [{Stats, ExPat, Ether} || {Stats, ExPat} <- parse_stat_entry(R, Ether)].

%% {Stat, {Type, Status, DPS}}

est([<<"type=", T/binary>> | Rest], _Type, Status, DPs) ->
  NewType = case T of
              <<"*">> -> '_';
              _ ->
                try binary_to_existing_atom(T, latin1)
                catch error:_ -> T
                end
            end, est(Rest, NewType, Status, DPs);
est([<<"status=", St/binary>> | Rest], Type, _Status, DPs) ->
  NewSt = case St of
            <<"enabled">> -> enabled;
            <<"disabled">> -> disabled;
            <<"*">> -> '_'
          end,
  est(Rest, Type, NewSt, DPs);
est([DPsBin | Rest], Type, Status, DPs) ->
  NewDPs = merge(
    [binary_to_existing_atom(D, latin1) || D <- re:split(DPsBin, ",")],
    DPs),
  est(Rest, Type, Status, NewDPs);
est([], Type, Status, DPs) ->
  {Type, Status, DPs}.

merge([_ | _] = DPs, default) ->
  DPs;
merge([H | T], DPs) ->
  case lists:member(H, DPs) of
    true -> merge(T, DPs);
    false -> merge(T, DPs ++ [H])
  end;
merge([], DPs) ->
  DPs.

parse_stat_entry([], {Type, Status}) ->
  {no_stat,
    {{[?PFX] ++ '_', Type, '_'}, [{'=:=', '$status', Status}], ['$_']}
  };
%% if the stat is an empty list it will make the exometer:select pattern to find
%% everything?
parse_stat_entry("*", {Type, Status}) ->
  parse_stat_entry([], {Type, Status});
parse_stat_entry("[" ++ _ = Expr, {_Type, _Status}) ->
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
  end; %% legacy Code
parse_stat_entry(Stat, {Type, Status})
  when Status == enabled; Status == disabled; Status == '_' ->
  Parts = re:split(Stat, "\\.", [{return, list}]),
  Heads = replace_parts(Parts),
  [{H, {{H, Type, Status}, [], ['$_']}} || H <- Heads];
parse_stat_entry(_Stat, {_Type, Status}) ->
  io:fwrite("(Illegal status : ~p~n", [Status]);
parse_stat_entry(Stat, {Type, Status, _DP}) ->
  parse_stat_entry(Stat, {Type, Status}).

ensure_trailing_dot(Str) ->
  case lists:reverse(Str) of
    "." ++ _ ->
      Str;
    _ ->
      Str ++ "."
  end.

partial_eval({cons, _, H, T}) ->
  [partial_eval(H) | partial_eval(T)];
partial_eval({tuple, _, Elems}) ->
  list_to_tuple([partial_eval(E) || E <- Elems]);
partial_eval({op, _, '++', L1, L2}) ->
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

split([H | T], H, Acc) ->
  {lists:reverse(Acc), T};
split([H | T], X, Acc) ->
  split(T, X, [H | Acc]);
split([], _, Acc) ->
  {lists:reverse(Acc), []}.

replace_parts_1([H | T]) ->
  R = replace_part(H),
  case T of
    ["**"] -> [R] ++ '_';
    _ -> [R | replace_parts_1(T)]
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
    [C | _] when C >= $0, C =< $9 ->
      try list_to_integer(H)
      catch
        error:_ -> list_to_atom(H)
      end;
    _ -> list_to_atom(H)
  end.


pads() ->
  [['_'],
    ['_', '_'],
    ['_', '_', '_'],
    ['_', '_', '_', '_'],
    ['_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_'],
    ['_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_']].


-spec(find_entries(data(), status()) ->  statlist() | error()).
%% @doc
%% pulls the information of a stat out of exometer
%% @end
find_entries(Arg, ToStatus) ->
  StatsList = parse_info(Arg, ToStatus),
  lists:foldl(fun({Stat, EPat, {Type, Status, DPs}}, Acc) ->
                  case legacy_search(Stat, Type, Status) of
                    false ->
                      [{find_entries_1(EPat), DPs} | Acc];
                    Found ->
                      [{Found, DPs} | Acc]
                  end
              end, [], StatsList).

find_entries_1(Pattern) ->
  riak_stat_coordinator:select(Pattern).

legacy_search(S, Type, Status) ->
  case re:run(S, "\\.", []) of
    {match, _} ->
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
          DPnames = [D || {D, _} <- DPs],
          case get_datapoint(Entry, DPnames) of
            {ok, Values} when is_list(Values) ->
              [{Entry, zip_values(Values, DPs)} | Acc];
            {ok, disabled} when Status == '_';
              Status == disabled ->
              [{Entry, zip_disabled(DPs)} | Acc];
            _ ->
              [{Entry, [{D, undefined} || D <- DPnames]} | Acc]
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


zip_values([{D, V} | T], DPs) ->
  {_, N} = lists:keyfind(D, 1, DPs),
  [{D, V, N} | zip_values(T, DPs)];
zip_values([], _) ->
  [].

zip_disabled(DPs) ->
  [{D, disabled, N} || {D, N} <- DPs].

repl([single | T]) ->
  <<"[^_]*", (repl(T))/binary>>;
repl([double | T]) ->
  <<".*", (repl(T))/binary>>;
repl([H | T]) ->
  <<H/binary, (repl(T))/binary>>;
repl([]) ->
  <<>>.

split_pattern(<<>>, Acc) ->
  lists:reverse(Acc);
split_pattern(<<"**", T/binary>>, Acc) ->
  split_pattern(T, [double | Acc]);
split_pattern(<<"*", T/binary>>, Acc) ->
  split_pattern(T, [single | Acc]);
split_pattern(B, Acc) ->
  case binary:match(B, <<"*">>) of
    {Pos, _} ->
      <<Bef:Pos/binary, Rest/binary>> = B,
      split_pattern(Rest, [Bef | Acc]);
    nomatch ->
      lists:reverse([B | Acc])
  end.


