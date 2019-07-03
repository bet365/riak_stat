%%%-------------------------------------------------------------------
%%% @author savannahallsop
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2019 14:40
%%%-------------------------------------------------------------------
-module(riak_stat_metadata).
-author("savannahallsop").

%% Basic API
-export([
  get_current_stats/0, get/2,
  put/3, put/4,
  delete/2]).

%% API
-export([
  check_meta/1, check_status/1,

  change_status/1, change_status/2]).

%% Admin API
-export([
  register_stat/4, register_stat/1,
  unregister/1,
  set_options/2, set_options/4,
  reset_stat/1]).

%% Profile API
-export([
  save_profile/1,
  load_profile/1,
  delete_profile/1,
  reset_profile/0]).

-define(PFX, riak_stat:prefix()).
-define(STAT, stats).
-define(PROF, profiles).
-define(NODEID, riak_core_nodeid:get()).
-define(PROFID, all).


%%%===================================================================
%%% Basic API
%%%===================================================================

get_current_stats() ->
  Stats = riak_stat_admin:get_stats(),
  lists:foldl(fun(Stat, Acc) ->
    Status = check_status(Stat),
    [{Stat, Status} | Acc]
              end, [], Stats).

get(Prefix, Key) ->
  riak_core_metadata:get(Prefix, Key).

put(Prefix, Key, Value) ->
  put(Prefix, Key, Value, []).

put(Prefix, Key, Value, Opts) ->
  riak_core_metadata:put(Prefix, Key, Value, Opts).

delete(Prefix, Key) ->
  riak_core_metadata:delete(Prefix, Key).

%%%===================================================================
%%% API
%%%===================================================================

-spec(check_meta(StatName :: metadata_key()) -> ok | term()).
%% @doc
%% returns the value of the stat from the metadata
%% @end
check_meta(StatName) ->
  case get({?NODEID, ?STAT}, StatName) of
    undefined ->
      undefined;
    [] ->
      undefined;
    Value ->
      case find_unregister_status(StatName, Value) of
        true ->
          lager:debug("Stat is unregistered: ~p~n", [StatName]),
          unregistered;
        false ->
          Value
      end
  end.

%%%%%%%%%% READING OPTS %%%%%%%%%%%%

-spec(check_status(StatName :: metadata_key()) -> term()).
%% @doc
%% Returns the status of the stat saved in the metadata
%% @end
check_status(StatName) ->
  case check_meta(StatName) of
    {{_NI, _S}, StatName, {_T, Opts, _A}} ->
      find_register_status([], Opts);
    Reason ->
      {error, Reason}
  end.

change_status(Stats) when is_list(Stats) ->
  lists:map(fun({Stat, {status, Status}}) ->
    change_status(Stat, Status)
            end, Stats);
change_status({StatName, Status}) ->
  change_status(StatName, Status).
-spec(change_status(Statname :: metadata_key(), ToStatus :: atom()) -> ok | term()).
%% @doc
%% Changes the status in the metadata
%% @end
change_status(Statname, ToStatus) ->
  Opts = stat_options(Statname),
  change_status_in_opts(Statname, ToStatus, Opts).

change_status_in_opts(Stat, NewStatus, Opts) ->
  NewOpts = lists:keyreplace(status, 1, Opts, {status, NewStatus}),
  set_options(Stat, NewOpts).

stat_options(StatName) ->
  get({?NODEID, ?STAT}, StatName).


%%%===================================================================
%%% Admin API
%%%===================================================================

%%%%%%%%%%%% REGISTERING %%%%%%%%%%%%

register_stat({StatName, Type, Opts, Aliases}) ->
  register_stat(StatName, Type, Opts, Aliases).
-spec(register_stat(StatName :: metadata_key(), Type :: atom() | term(), Opts :: list(), Aliases :: term()) ->
  ok | term() | {error, Reason :: term()}).
%% @doc
%% Checks if the stat is already registered in the metadata, if not it
%% registers it, and pulls out the options for the status and sends it
%% back to go into exometer
%% @end
register_stat(StatName, Type, Opts, Aliases) ->
  case check_meta(StatName) of % check registration
    undefined -> % if not registered return default Opts
      re_register_stat(StatName, Type, [{vclock, vclock:fresh(?NODEID, 1)} | Opts], Aliases),
      Opts;
    {_Type, MetaOpts, _Aliases} -> % if registered
      find_register_status(Opts, MetaOpts);
    _ ->
      lager:debug("riak_stat_meta_mgr:register_stat --
      Could not register the stat:~n{{~p,~p},~p,{~p,~p,~p}}~n",
        [?NODEID, ?STAT, StatName, Type, Opts, Aliases])
  end.

re_register_stat(StatName, Type, Opts, Aliases) ->
  put({?NODEID, ?STAT}, StatName, {Type, Opts, Aliases}),
  ok.

find_register_status(NewOpts, MetaOpts) ->
  case lists:keyfind(status, 1, MetaOpts) of
    false ->
      NewOpts;
    Status -> % {status, disabled}
      [Status | NewOpts]
  end.

find_unregister_status(_SN, {_T, Opts, _A}) ->
  case lists:keyfind(unregistered, 1, Opts) of
    false ->
      false;
    {unregistered, Bool} ->
      Bool
  end.

%%%%%%%%%% UNREGISTERING %%%%%%%%%%%%

-spec(unregister(StatName :: term()) -> ok | term()).
%% @doc
%% Marks the stats as unregistered, that way when a node is restarted and registers the
%% stats it will ignore stats that are marked unregistered
%% @end
unregister(Statname) ->
  case check_meta(Statname) of
    unregistered ->
      lager:info("Stat is unregistered: ~p~n", [Statname]),
      unregistered;
    {Type, MetaOpts, Aliases} ->
      NewOpts = lists:keyreplace(unregistered, 1, MetaOpts, {unregistered, true}),
      set_options(Statname, Type, NewOpts, Aliases),
      ok;
    _ ->
      ok
  end.

%%%%%%%%%% SET OPTIONS %%%%%%%%%%%%%

-spec(set_options(StatName :: metadata_key(), NewOpts :: list() | tuple()) ->
  ok | term() | {error, Reason :: term()}).
%% @doc
%% Setting the options in the metadata manually, such as {unregistered, false | true} etc...
%% @end
set_options(Statname, NewOpts) when is_list(NewOpts) ->
  lists:foreach(fun({Key, NewVal}) ->
    set_options(Statname, {Key, NewVal})
                end, NewOpts);
set_options(Statname, {Key, NewVal}) ->
  case check_meta(Statname) of
    undefined ->
      io:fwrite("Stat is not registered: ~p~n", [Statname]);
    unregistered ->
      io:fwrite("Stat is unregistered: ~p~n", [Statname]);
    {Type, Opts, Aliases} ->
      NewOpts = lists:keyreplace(Key, 1, Opts, {Key, NewVal}),
      NewOpts2 = fresh_clock(NewOpts),
      set_options(Statname, Type, NewOpts2, Aliases)
  end.

set_options(StatName, Type, NewOpts, Aliases) ->
  re_register_stat(StatName, Type, NewOpts, Aliases).

%%%%%%%%% RESETTING %%%%%%%%%%%

-spec(reset_stat(Statname :: metadata_key()) -> ok | term()).
%% @doc
%% reset the stat in exometer and notify exometer of its reset
%% @end
reset_stat(Statname) ->
  case check_meta(Statname) of
    undefined ->
      io:fwrite("Stat is not registered: ~p~n", [Statname]);
    unregistered ->
      io:fwrite("Stat is unregistered: ~p~n", [Statname]);
    {Type, Opts, Aliases} ->
      {value, {resets, Resets}} = lists:keysearch(resets, 1, Opts),
      NewOpts1 = change_status(Statname, enabled),
      set_options(Statname, Type,
        lists:keyreplace(resets, 1, NewOpts1, {resets, reset_inc(Resets)}), Aliases)
  end.

fresh_clock(Opts) ->
  case lists:keysearch(vclock, 1, Opts) of
    false ->
      [{vclock, clock_fresh(?NODEID, 0)} | Opts];
    {value, {vclock, [{Node, {Count, _VC}}]}} ->
      lists:keyreplace(vclock, 1, Opts, {vclock, clock_fresh(Node, Count)});
    _ ->
      [{vclock, clock_fresh(?NODEID, 0)} | Opts]
  end.

reset_inc(Count) -> Count + 1.

clock_fresh(Node, Count) ->
  vclock:fresh(Node, vc_inc(Count)).
vc_inc(Count) -> Count + 1.



%%%===================================================================
%%% Profile API
%%%===================================================================

-spec(save_profile(ProfileName :: atom()) ->
  term() | {error, Reason :: term()}).
%% @doc
%% Take the stats and their status out of the metadata for the current
%% node and save it into the metadata as a profile
%% @end
save_profile(ProfileName) ->
  Stats = get_current_stats(),
  register_profile(ProfileName, Stats).

-spec(load_profile(ProfileName :: atom()) ->
  term() | {error, Reason :: term()}).
%% @doc
%% Find the profile in the metadata and pull out stats to change them
%% It will compare the current stats with the profile stats and will
%% change the ones that need changing to prevent errors
%% @end
load_profile(ProfileName) ->
  CurrentStats = get_current_stats(),
  ProfileStats = get_profile_stats(ProfileName),
  ToChange = compare_stats(CurrentStats, ProfileStats),
  change_stat_status(ToChange).

-spec(delete_profile(ProfileName :: atom()) ->
  term() | {error, Reason :: term()}).
%% @doc
%% Deletes the profile from the metadata, however currently the metadata
%% returns a tombstone for the profile, it can be overwritten when a new profile
%% is made of the same name, and in the profile gen_server the name of the
%% profile is "unregistered" so it can not be reloaded again after deletion
%% @end
delete_profile(ProfileName) ->
  delete_p(ProfileName).

-spec(reset_profile() -> ok | term() | {error, Reason :: term()}).
%% @doc
%% resets the profile by enabling all the stats, pulling out all the stats that
%% are disabled in the metadata and then changing them to enabled in both the
%% metadata and exometer
%% @end
reset_profile() ->
  CurrentStats = get_current_stats(),
  DisabledStats = find_stats(CurrentStats, disabled),
  change_stat_status(DisabledStats).

%%%%% ----------------------------------------------- %%%%%%

register_profile(ProfileName, Stats) ->
  put({?PROFID, ?PROF}, ProfileName, Stats),
  io:fwrite("Profile Stats stored in metadata ~n").

get_profile_stats(ProfileName) ->
  get({?PROFID, ?PROF}, ProfileName).

compare_stats(CurrentMetaStats, ProfileStats) ->
  lists:foldl(fun({Stat, Status}, Acc) ->
    [find_diff({Stat, Status}, CurrentMetaStats) | Acc]
              end, [], ProfileStats).

find_diff({Stat, Status}, List) ->
  case lists:keyfind(Stat, 1, List) of
    false ->
      [];
    {Stat, MetaStatus} when Status == MetaStatus ->
      [];
    {Stat, MetaStatus} when Status =/= MetaStatus ->
      [{Stat, Status}]
  end.

find_stats(Stats, Status) ->
  lists:foldl(
    fun({Stat, St}, Acc) when St == Status ->
      [{Stat, St} | Acc];
      ({_Stat, St}, Acc) when St =/= Status ->
        Acc
    end, [], Stats).

change_stat_status(Stats) ->
  ToChange =
    lists:foldl(fun({Stat, Status}, Acc) ->
      NewSt =
        case Status of
          enabled -> disabled;
          disabled -> enabled;
          Arg -> Arg
        end,
      [{Stat, {status, NewSt}} | Acc]
                end, [], Stats),
  riak_stat_coordinator:coordinate(change_status, ToChange).

delete_p(Profile) ->
  delete({?PROFID, ?PROF}, Profile).


%% the code you see below is not as necessary as I thought





%%  case check_status(Statname) of
%%    DiffStatus when DiffStatus =/= {status, ToStatus} ->
%%      set_options(Statname, {status, ToStatus});
%%    DiffStatus when DiffStatus == ToStatus ->
%%      {error, no_change}
%%  end.

% todo: make it less expensive, so it doesn't keep calling into the metadata
% also make it so it returns the stat and the status in a tuple.









%%change_stat_status(Stats) ->
%%  MetaStats = get_profile_stats(Profile),
%%  ChangeStatus =
%%    lists:foldl(fun(Stat, StatsAcc) ->
%%      case check_prof_meta(Profile, Stat) of
%%        [{Key, disabled}] ->
%%          [{Key, {status, enabled}} | StatsAcc];
%%        [{Key, enabled}] ->
%%          [{Key, {status, disabled}} | StatsAcc];
%%        _ ->
%%          StatsAcc
%%      end end, [], Stats),
%%  change_these_stats(ChangeStatus),
%%  FinalStats = the_alpha_stat(ChangeStatus, MetaStats),
%%  add_profile(Profile, FinalStats).


%%load_profile(ProfileName) ->
%%  case check_profile_stat(ProfileName, '_') of
%%    {error, Reason} ->
%%      io:fwrite("error: ~p~n", [Reason]);
%%    Stats ->
%%      change_these_stats(Stats)
%%  end.
%%
%%-spec(reset_profile(ProfileName :: term()) -> ok | term()).
%%%% @doc
%%%% takes the stats that are disabled out of the metadata and returns them to
%%%% the stat_mngr so they can be re-enabled, the profile is then removed
%%reset_profile(ProfileName) ->
%%  % return the disabled stats
%%  check_profile_stat(ProfileName, {'_', {status, disabled}}).
%%
%%-spec(add_profile(ProfileName :: term(), Stats :: list()) -> ok | term()).
%%%% @doc
%%%% add a profile into the metadata with a list of stats as its value, inside
%%%% the stats value is the status of said stat, upon loading of the profile the
%%%% status is checked and changed in exometer
%%%% @end
%%add_profile(ProfileName, Stats) ->
%%  register_profile(ProfileName, Stats).
%%
%%
%%-spec(remove_profile(ProfileName :: term()) -> term()).
%%%% @doc
%%%% remove the profile from the metadata - the metadata doesn't delete
%%%% any actual data, it leaves behind a tombstone
%%%% @end
%%remove_profile(ProfileName) ->
%%  tombstone_profile(ProfileName).


%%change_these_stats(Stats) ->
%%  riak_stat_mngr:change_these_stats(Stats).
%%
%%
%%
%%check_prof_meta(ProfileName, Stat) ->
%%  case riak_core_metadata:get({riak_stat, ?PROF}, ProfileName) of
%%    undefined ->
%%      undefined;
%%    [] ->
%%      undefined;
%%    Value ->
%%      case find_profile_stat_status(Value, Stat) of
%%        false ->
%%          [];
%%        Status ->
%%          [{Stat, Status}]
%%      end
%%  end.
%%
%%find_profile_stat_status({{_RS, _PR}, _PN, Stats}, Stat) ->
%%  case lists:keyfind(Stat, 1, Stats) of
%%    false ->
%%      false;
%%    {Stat, {status, Status}} ->
%%      Status
%%  end.




%%the_alpha_stat(Stats1, Stats2) ->
%%  riak_stat_mngr:the_alpha_stat(Stats1, Stats2).
%%
%%meta_keyer(Stats) ->
%%  lists:map(fun({Key, Value}) ->
%%    meta_keyer(Key, Value)
%%            end, Stats).
%%
%%meta_keyer(Key, Value) ->
%%  riak_stat_mngr:meta_keyer(Key, Value).


%% register_stats -> exometer
%% pull out the options and send to exometer, checking isnt really necessary

%% make Primary functions calls to the metadata

%% secondary calls, calls necessary for specific funstions or data

%% helper functions