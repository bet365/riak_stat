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
-export([register_stat/4, alias/1, aliases/2, % Create
        re_register/2, re_register/3,

        get_datapoint/2, get_value/1, get_values/1, select_stat/1,  % Read
        info/2,

        update_or_create/3, update_or_create/4, set_opts/2, % Update

        unregister_stat/1, reset_stat/1 % Delete
]).

%% Secondary API
-export([timestamp/0, coordinate/1
        ]).

%% additional API
-export([start/0, stop/0]).

%%%%%%%%%%%%%% CREATING %%%%%%%%%%%%%%


coordinate({Fun, Arg}) ->
  coordinate(Fun, Arg).
coordinate(Fun, Arg) ->
  case Fun of
    register ->
      {StatName, Type, Opts, Aliases} = Arg,
      register_stat(StatName, Type, Opts, Aliases);
    update ->
      {Name, UpdateVal, Type} = Arg,
      update_or_create(Name, UpdateVal, Type)
  end.


-spec(register_stat(StatName :: list(), Type :: atom(), Opts :: list(), Aliases :: term()) ->
  ok | {error, Reason :: term()}).
%% @doc
%% Registers all stats, using exometer:re_register/3, any stat that is
%% re_registered overwrites the previous entry, works the same as
%% exometer:new/3 except it will ont return an error if the stat already
%% is registered.
%% @end
register_stat(StatName, Type, Opts, Aliases) ->
  re_register(StatName, Type, Opts), %% returns -> ok.
  lists:foreach(
    fun({DP, Alias}) ->
      aliases(new, [Alias, StatName, DP]) %% returns -> ok | {error, Reason}
    end, Aliases).

re_register(StatName, Type) ->
  re_register(StatName, Type, []).

re_register(StatName, Type, Opts) ->
  exometer:re_register(StatName, Type, Opts).

-spec(alias(Group :: term()) -> ok | term()).
alias(Group) ->
  lists:keysort(
    1,
    lists:foldl(
      fun({K, DPs}, Acc) ->
        case get_datapoint(K, [D || {D,_} <- DPs]) of
          {ok, Vs} when is_list(Vs) ->
            lists:foldr(fun({D,V}, Acc1) ->
              {_,N} = lists:keyfind(D,1,DPs),
              [{N,V}|Acc1]
                        end, Acc, Vs);
          Other ->
            Val = case Other of
                    {ok, disabled} -> undefined;
                    _ -> 0
                  end,
            lists:foldr(fun({_,N}, Acc1) ->
              [{N,Val}|Acc1]
                        end, Acc, DPs)
        end
      end, [], orddict:to_list(Group))).

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

%%%%%%%%%%%%%% READING %%%%%%%%%%%%%%

-spec(get_datapoint(Name :: term(), Datapoint :: term()) -> term()).
%% @doc
%% Retrieves the datapoint value from exometer
%% @end
get_datapoint(Name, Datapoint) ->
  exometer:get_value(Name, Datapoint).

-spec(get_value(Stat :: list()) -> ok | term()).
%% @doc
%% Same as the function above, except in exometer the Datapoint:
%% 'default' is inputted, however it is used by some modules
%% @end
get_value(S) ->
  exometer:get_value(S).

-spec(get_values(Path :: any()) -> term()).
%% @doc
%% The Path is the start or full name of the stat(s) you wish to find,
%% i.e. [riak,riak_kv] as a path will return stats with those to elements
%% in their path. and uses exometer:find_entries and above function
%% @end
get_values(Path) ->
  exometer:get_values(Path).

-spec(select_stat(Pattern :: term()) -> term()).
%% @doc
%% Find the stat in exometer using this pattern
%% @end
select_stat(Pattern) ->
  exometer:select(Pattern).

-spec(info(Name :: list() | term(), Type :: atom() | term()) -> term()).
%% @doc
%% find information about a stat on a specific item
%% @end
info(Name, Type) ->
  exometer:info(Name, Type).

%%%%%%%%%%%%%% UPDATING %%%%%%%%%%%%%%

-spec(update_or_create(Name :: term(), UpdateVal :: any(), Type :: atom() | term()) ->
  ok | term()).
%% @doc
%% Sends the stat to exometer to get updated, unless it is not already a stat then it
%% will be created. First it is checked in meta_mgr and registered there.
%% @end
update_or_create(Name, UpdateVal, Type) ->
  update_or_create(Name, UpdateVal, Type, []).
-spec(update_or_create(Name :: list() | atom(), UpdateVal :: any(), Type :: atom() | term(), Opts :: list()) ->
  ok | term()).
update_or_create(Name, UpdateVal, Type, Opts) ->
  exometer:update_or_create(Name, UpdateVal, Type, Opts).

-spec(set_opts(StatName :: list() | atom(), Opts :: list()) -> ok | term()).
%% @doc
%% Set the options for a stat in exometer, setting the status as either enabled or
%% disabled in it's options in exometers will change its status in the entry
%% @end
set_opts(StatName, Opts) ->
  exometer:setopts(StatName, Opts).

%%%%%%%%%%%%% UNREGISTER / RESET %%%%%%%%%%%%%%

-spec(unregister_stat(StatName :: term()) -> ok | term() | {error, Reason :: term()}).
%% @doc
%% deletes the stat entry from exometer
%% @end
unregister_stat(StatName) ->
  exometer:delete(StatName).

-spec(reset_stat(StatName :: term()) -> ok | term()).
%% @doc
%% resets the stat in exometer
%% @end
reset_stat(StatName) ->
  exometer:reset(StatName).

%%%%%%%%%%%% Helper Functions %%%%%%%%%%%

-spec(timestamp() -> term()).
%% @doc
%% Returns the timestamp to put in the stat entry
%% @end
timestamp() ->
  exometer_util:timestamp().




%%%%%%%%%%%% Extras %%%%%%%%%%%%%%%


%% @doc
%% Used in testing in certain modules
%% @end
start() ->
  exometer:start().

stop() ->
  exometer:stop().








%%select(Pattern) ->
%%  exometer:select(Pattern).
%%
%%find_entries({Arg, Status}) ->
%%  find_entries(Arg, Status).
%%
%%find_entries(Arg, ToStatus) ->
%%  lists:map(fun(Stat) ->
%%%%    {S, Type, Status, DPs} = type_status_and_dps(Stat, ToStatus),
%%%%    case Stat of
%%%%      "[" ++ _ ->
%%%%        {find_patterns(S, Type, Status), DPs};
%%%%      _ ->
%%
%%    %% TODO: call into type_status_and_dps
%%    Type = '_',
%%    DPs = default,
%%        case legacy_search(Stat, Type, ToStatus) of
%%          false ->
%%            {find_patterns(Stat, Type, ToStatus), DPs};
%%          Found ->
%%            {Found, DPs}
%%        end
%%%%    end
%%            end, Arg).
%%
%%
%%
%%find_patterns(Stat, Type, Status) ->
%%  Pattern = lists:flatten([riak_stat_data:sani_data_for_patterns(Stat, Type, Status)]),
%%  select(Pattern).
%%
%%%% TODO: change this for the new Arg type
%%legacy_search(S, Type, Status) ->
%%  case re:run(S, "\\.", []) of
%%    {match,_} ->
%%      false;
%%    nomatch ->
%%      Re = <<"^", (make_re(S))/binary, "$">>,
%%      [{S, legacy_search_1(Re, Type, Status)}]
%%  end.
%%
%%make_re(S) ->
%%  repl(split_pattern(S, [])).
%%
%%legacy_search_1(N, Type, Status) ->
%%  Found = aliases(regexp_foldr, [N]),
%%  lists:foldr(
%%    fun({Entry, DPs}, Acc) ->
%%      case match_type(Entry, Type) of
%%        true ->
%%          DPnames = [D || {D,_} <- DPs],
%%          case get_datapoint(Entry, DPnames) of
%%            {ok, Values} when is_list(Values) ->
%%              [{Entry, zip_values(Values, DPs)} | Acc];
%%            {ok, disabled} when Status=='_';
%%              Status==disabled ->
%%              [{Entry, zip_disabled(DPs)} | Acc];
%%            _ ->
%%              [{Entry, [{D,undefined} || D <- DPnames]}|Acc]
%%          end;
%%        false ->
%%          Acc
%%      end
%%    end, [], orddict:to_list(Found)).
%%
%%match_type(_, '_') ->
%%  true;
%%match_type(Name, T) ->
%%  T == get_info(Name, type).
%%
%%zip_values([{D,V}|T], DPs) ->
%%  {_,N} = lists:keyfind(D, 1, DPs),
%%  [{D,V,N}|zip_values(T, DPs)];
%%zip_values([], _) ->
%%  [].
%%
%%zip_disabled(DPs) ->
%%  [{D,disabled,N} || {D,N} <- DPs].
%%
%%repl([single|T]) ->
%%  <<"[^_]*", (repl(T))/binary>>;
%%repl([double|T]) ->
%%  <<".*", (repl(T))/binary>>;
%%repl([H|T]) ->
%%  <<H/binary, (repl(T))/binary>>;
%%repl([]) ->
%%  <<>>.
%%
%%split_pattern(<<>>, Acc) ->
%%  lists:reverse(Acc);
%%split_pattern(<<"**", T/binary>>, Acc) ->
%%  split_pattern(T, [double|Acc]);
%%split_pattern(<<"*", T/binary>>, Acc) ->
%%  split_pattern(T, [single|Acc]);
%%split_pattern(B, Acc) ->
%%  case binary:match(B, <<"*">>) of
%%    {Pos,_} ->
%%      <<Bef:Pos/binary, Rest/binary>> = B,
%%      split_pattern(Rest, [Bef|Acc]);
%%    nomatch ->
%%      lists:reverse([B|Acc])
%%  end.
%%
%%get_info(Name, Info) ->
%%  case riak_stat_exometer:get_info(Name, Info) of
%%    undefined ->
%%      [];
%%    Other ->
%%      Other
%%  end.
%%
%%-spec(aliases(Type :: atom(), Entry :: term()) -> ok | term()).
%%%% @doc
%%%% goes to exometer_alias and performs the type of alias function specified
%%%% @end
%%aliases(new, [Alias, StatName, DP]) ->
%%  exometer_alias:new(Alias, StatName, DP);
%%aliases(prefix_foldl, []) ->
%%  exometer_alias:prefix_foldl(<<>>, alias_fun(), orddict:new());
%%aliases(regexp_foldr, [N]) ->
%%  exometer_alias:regexp_foldr(N, alias_fun(), orddict:new()).
%%
%%alias_fun() ->
%%  fun(Alias, Entry, DP, Acc) ->
%%    orddict:append(Entry, {DP, Alias}, Acc)
%%  end.
%%
%%
%%get_datapoint(Name, DP) ->
%%  exometer:get_value(Name, DP).


%%legacy_search(Stat, Type, ToStatus) ->
%%  riak_stat_data:legacy_search(Stat, Type, ToStatus).