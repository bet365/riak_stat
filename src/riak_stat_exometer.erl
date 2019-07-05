%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2019 14:40
%%%-------------------------------------------------------------------
-module(riak_stat_exometer).
-author("savannahallsop").

%% API
-export([
  register_stat/4, alias/1, aliases/2, re_register/2,
  read_stats/1, get_datapoint/2, get_value/1, get_values/1, select_stat/1, info/2,
  update_or_create/3, update_or_create/4, change_status/1, set_opts/2,
  unregister_stats/4, unregister_stat/1, reset_stat/1
]).

%% Secondary API
-export([timestamp/0]).

%% additional API
-export([start/0, stop/0]).

-type statname()  :: atom() | list().
-type type()      :: atom() | tuple().
-type options()   :: list().
-type aliases()   :: list() | atom() | tuple().
-type datapoint() :: list() | atom() | tuple().
-type value()     :: any().
-type exo_value() :: {ok, value()}.
-type reason()    :: no_template | exists | not_found | any().
-type error()     :: {error, reason()}.
-type acc()       :: any().
-type pattern()   :: ets:match_spec().
-type timestamp() :: non_neg_integer().
-type info() :: name | type | module | value | cache
| status | timestamp | options | ref | datapoints | entry.


-define(PFX, riak_stat:prefix()).

%%%%%%%%%%%%%% CREATING %%%%%%%%%%%%%%

-spec(register_stat(statname(), type(), options(), aliases()) ->
  ok | error()).
%% @doc
%% Registers all stats, using exometer:re_register/3, any stat that is
%% re_registered overwrites the previous entry, works the same as
%% exometer:new/3 except it wont return an error if the stat already
%% is registered.
%% @end
register_stat(StatName, Type, Opts, Aliases) ->
  lists:foreach(
    fun({DP, Alias}) ->
      aliases(new, [Alias, StatName, DP]) %% returns -> ok | {error, Reason}
    end, Aliases),
  re_register(StatName, Type, Opts). %% returns -> ok.

re_register(StatName, Type) ->
  re_register(StatName, Type, []).

re_register(StatName, Type, Opts) ->
  exometer:re_register(StatName, Type, Opts).


-spec(alias(Group :: term()) -> ok | acc()).
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

-spec(aliases(Type :: atom(), Entry :: list()) -> ok | acc() | error()).
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

-spec(read_stats(App :: atom()) -> value() | error()).
%% @doc
%% read the stats from exometer and print them out in the format needed, uses
%% exometer functions.
%% @end
read_stats(App) ->
  Values = get_values([?PFX, App]),
  [print_stats(Name, [status]) || {Name, _V} <- Values].

-spec(get_datapoint(statname(), datapoint()) -> exo_value() | error()).
%% @doc
%% Retrieves the datapoint value from exometer
%% @end
get_datapoint(Name, Datapoint) ->
  exometer:get_value(Name, Datapoint).

-spec(get_value(statname()) -> exo_value() | error()).
%% @doc
%% Same as the function above, except in exometer the Datapoint:
%% 'default' is inputted, however it is used by some modules
%% @end
get_value(S) ->
  exometer:get_value(S).

-spec(get_values(Path :: any()) -> exo_value() | error()).
%% @doc
%% The Path is the start or full name of the stat(s) you wish to find,
%% i.e. [riak,riak_kv] as a path will return stats with those to elements
%% in their path. and uses exometer:find_entries and above function
%% @end
get_values(Path) ->
  exometer:get_values(Path).

-spec(select_stat(pattern()) -> list()).
%% @doc
%% Find the stat in exometer using this pattern
%% @end
select_stat(Pattern) ->
  exometer:select(Pattern).

-spec(info(statname(), info()) -> value()).
%% @doc
%% find information about a stat on a specific item
%% @end
info(Name, Type) ->
  exometer:info(Name, Type).

%%%%%%%%%%%%%% UPDATING %%%%%%%%%%%%%%

-spec(update_or_create(statname(), value(), type()) ->
  ok | error()).
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

-spec(change_status(Stats :: list() | term()) ->
  ok | term()).
%% @doc
%% enable or disable the stats in the list
%% @end
change_status(Stats) when is_list(Stats) ->
  lists:map(fun
              ({Stat, {status, Status}}) -> change_status(Stat, Status);
              ({Stat, Status}) ->           change_status(Stat, Status)
            end, Stats);
change_status({Stat, Status}) ->
  change_status(Stat, Status).
change_status(Stat, Status) ->
  set_opts(Stat, [{status, Status}]).


-spec(set_opts(statname(), options()) -> ok | error()).
%% @doc
%% Set the options for a stat in exometer, setting the status as either enabled or
%% disabled in it's options in exometer will change its status in the entry
%% @end
set_opts(StatName, Opts) ->
  exometer:setopts(StatName, Opts).

%%%%%%%%%%%%% UNREGISTER / RESET %%%%%%%%%%%%%%

unregister_stats([Op, time], Idx, Type, App) ->
  unregister_stat([?PFX, App, Type, Op, time, Idx]);
unregister_stats(Mod, Idx, Type, App) ->
  unregister_stat([?PFX, App, Type, Mod, Idx]).

-spec(unregister_stat(statname()) -> ok | error()).
%% @doc
%% deletes the stat entry from exometer
%% @end
unregister_stat(StatName) ->
  exometer:delete(StatName).

-spec(reset_stat(statname()) -> ok | error()).
%% @doc
%% resets the stat in exometer
%% @end
reset_stat(StatName) ->
  exometer:reset(StatName).

%%%%%%%%%%%% Helper Functions %%%%%%%%%%%

-spec(timestamp() -> timestamp()).
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

%% ADMIN

print_stats(Stat, Attr) ->
  riak_stat_info:print(Stat, Attr).