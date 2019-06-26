%%%-------------------------------------------------------------------
%%% @author savannahallsop
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2019 14:40
%%%-------------------------------------------------------------------
-module(riak_stat_exometer).
-author("savannahallsop").

%% Primary API
-export([select/1]).

%% Secondary API
-export([find_entries/1]).

%% helper API
-export([]).

select(Pattern) ->
  exometer:select(Pattern).

find_entries({Arg, Status}) ->
  find_entries(Arg, Status).

find_entries(Arg, ToStatus) ->
  lists:map(fun(Stat) ->
%%    {S, Type, Status, DPs} = type_status_and_dps(Stat, ToStatus),
%%    case Stat of
%%      "[" ++ _ ->
%%        {find_patterns(S, Type, Status), DPs};
%%      _ ->

    %% TODO: call into type_status_and_dps
    Type = '_',
    DPs = default,
        case legacy_search(Stat, Type, ToStatus) of
          false ->
            {find_patterns(Stat, Type, ToStatus), DPs};
          Found ->
            {Found, DPs}
        end
%%    end
            end, Arg).



find_patterns(Stat, Type, Status) ->
  Pattern = lists:flatten([riak_stat_data:sani_data_for_patterns(Stat, Type, Status)]),
  select(Pattern).

%% TODO: change this for the new Arg type
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

get_info(Name, Info) ->
  case riak_stat_exometer:get_info(Name, Info) of
    undefined ->
      [];
    Other ->
      Other
  end.

-spec(aliases(Type :: atom(), Entry :: term()) -> ok | term()).
%% @doc
%% goes to exometer_alias and performs the type of alias function specified
%% @end
aliases(new, [Alias, StatName, DP]) ->
  exometer_alias:new(Alias, StatName, DP);
aliases(prefix_foldl, []) ->
  exometer_alias:prefix_foldl(<<>>, alias_fun(), orddict:new());
aliases(regexp_foldr, [N]) ->
  exometer_alias:regexp_foldr(N, alias_fun(), orddict:new()).

alias_fun() ->
  fun(Alias, Entry, DP, Acc) ->
    orddict:append(Entry, {DP, Alias}, Acc)
  end.


get_datapoint(Name, DP) ->
  exometer:get_value(Name, DP).


%%legacy_search(Stat, Type, ToStatus) ->
%%  riak_stat_data:legacy_search(Stat, Type, ToStatus).