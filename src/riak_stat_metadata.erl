%%%-------------------------------------------------------------------
%%% @doc
%%% riak_stat_metadata is the middleman for riak_stat and
%%% riak_core_metadata. All information that needs to go into or out
%%% of the metadata will always go through this module.
%%% @end
%%% Created : 25. Jun 2019 14:40
%%%-------------------------------------------------------------------
-module(riak_stat_metadata).
-author("savannahallsop").

-include("riak_core_metadata.hrl").

%% TODO START

% - create a function that will reset all the resets in the metadata stats

% - Create a Eunit test for the Api
% - create a Eunit test for the basic Api
% - create a Eunit test for the Profile Api

%% TODO END

%% Basic API
-export([
  get/2,
  put/3, put/4,
  delete/2
]).

%% API
-export([
  check_meta/1, check_status/1,
  change_status/1, change_status/2,
  set_options/2, set_options/4
]).

%% Admin API
-export([
  register_stat/4, register_stat/1,
  unregister/1,
  reset_stat/1
]).

%% Profile API
-export([
  get_current_stats/0,
  save_profile/1,
  load_profile/1,
  delete_profile/1,
  reset_profile/0]).

-define(PFX, riak_stat:prefix()).
-define(STAT, stats).
-define(PROF, profiles).
-define(NODEID, riak_core_nodeid:get()).
-define(PROFID, all).
-define(STATPFX, {?NODEID, ?STAT}).
-define(PROFPFX, {?PROFID, ?PROF}).
-define(STATKEY(StatName), {?STATPFX, StatName}).
-define(PROFILEKEY(Profile), {?PROFPFX, Profile}).

%%%===================================================================
%%% Basic API
%%%===================================================================

-spec(get(Prefix :: metadata_prefix(), Key :: metadata_key()) -> metadata_value()).
%% @doc Get the data from the riak_core_metadata @end
get(Prefix, Key) ->
  riak_core_metadata:get(Prefix, Key).

-spec(put(Prefix :: metadata_prefix(), Key :: metadata_key(),
    Value :: metadata_value(), Opts :: term()) -> ok).
%% @doc put the data into the metadata @end
put(Prefix, Key, Value) ->
  put(Prefix, Key, Value, []).
put(Prefix, Key, Value, Opts) ->
  riak_core_metadata:put(Prefix, Key, Value, Opts).

-spec(delete(Prefix :: metadata_prefix(), Key :: metadata_key()) -> ok).
%% @doc deleting the key from the metadata replaces values with tombstone @end
delete(Prefix, Key) ->
  riak_core_metadata:delete(Prefix, Key).

%%%===================================================================
%%% API
%%%===================================================================

-spec(check_meta(PrefixKey :: metadata_pkey()) -> metadata_value()).
%% @doc
%% Checks the metadata for the pkey provided
%% returns [] | Value
%% @end
check_meta({Prefix, Key}) ->
  case get(Prefix, Key) of
    undefined -> % Not found, return empty list
      [];
    Value ->
      case find_unregister_status(Key, Value) of
        false ->
          Value;
        unregistered -> unregistered;
        _ -> Value
      end
  end.

find_unregister_status(_K, '$deleted') ->
  unregistered;
find_unregister_status(_SN, {_T, Opts, _A}) ->
  case lists:keyfind(status, 1, Opts) of
    false ->
      false;
    {status, Status} ->
      Status % enabled | disabled =/= unregistered
  end;
find_unregister_status(_PN, _Stats) ->
  false.

%%%%%%%%%% READING OPTS %%%%%%%%%%%%

-spec(check_status(StatName :: metadata_key()) -> metadata_value() | {error, Reason :: term()}).
%% @doc
%% Returns the status of the stat saved in the metadata
%% @end
check_status(StatName) ->
  case check_meta(?STATPFX(StatName)) of
    [] ->
      {error, no_stat};
    unregistered ->
      {error, no_status};
    Value ->
      Status = find_unregister_status(StatName, Value),
          {StatName, {status, Status}}
  end.



-spec(change_status(Statname :: metadata_key(), ToStatus :: atom()) -> ok | term()).
%% @doc
%% Changes the status in the metadata
%% @end
change_status(Stats) when is_list(Stats) ->
  lists:map(fun
              ({Stat, {status, Status}}) -> change_status(Stat, Status);
              ({Stat, Status}) -> change_status(Stat, Status)
            end, Stats);
change_status({StatName, Status}) ->
  change_status(StatName, Status).
change_status(Statname, ToStatus) ->
%%  {_T, Opts, _A} =
    case check_meta(?STATKEY(Statname)) of
      [] ->
        ok;
      unregistered ->
        ok;
      {Type, Opts, Aliases} ->
        change_status_in_opts({Statname, Type, Opts, Aliases}, ToStatus)
    end.

change_status_in_opts(StatInfo, NewStatus) ->
  NewOpts = {status, NewStatus},
  set_options(StatInfo, NewOpts).


%%%%%%%%%% SET OPTIONS %%%%%%%%%%%%%

-spec(set_options(StatName :: metadata_key(), NewOpts :: list() | tuple()) ->
  ok | term() | {error, Reason :: term()}).
%% @doc
%% Setting the options in the metadata manually, such as
%% {status, enabled | disabled | unregistered} etc...
%% @end
set_options(StatInfo, NewOpts) when is_list(NewOpts) ->
  lists:foreach(fun({Key, NewVal}) ->
    set_options(StatInfo, {Key, NewVal})
                end, NewOpts);
set_options({Statname, Type, Opts, Aliases}, {Key, NewVal}) ->
      NewOpts = lists:keyreplace(Key, 1, Opts, {Key, NewVal}),
      NewOpts2 = fresh_clock(NewOpts),
      set_options(Statname, Type, NewOpts2, Aliases).

set_options(StatName, Type, NewOpts, Aliases) ->
  re_register_stat(StatName, Type, NewOpts, Aliases).

fresh_clock(Opts) ->
  case lists:keysearch(vclock, 1, Opts) of
    false ->
      [{vclock, clock_fresh(?NODEID, 0)} | Opts];
    {value, {vclock, [{Node, {Count, _VC}}]}} ->
      lists:keyreplace(vclock, 1, Opts, {vclock, clock_fresh(Node, Count)});
    _ ->
      [{vclock, clock_fresh(?NODEID, 0)} | Opts]
  end.

clock_fresh(Node, Count) ->
  vclock:fresh(Node, vc_inc(Count)).
vc_inc(Count) -> Count + 1.

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
  case check_meta(?STATPFX(StatName)) of % check registration
    [] -> % if not registered return default Opts
      re_register_stat(StatName, Type, [{vclock, vclock:fresh(?NODEID, 1)} | Opts], Aliases),
      Opts;
    unregistered -> {error, unregistered};
    {_Type, MetaOpts, _Aliases} -> % if registered
      [find_status(MetaOpts) | Opts];
    _ ->
      lager:debug("riak_stat_meta_mgr:register_stat --
      Could not register the stat:~n{{~p,~p},~p,{~p,~p,~p}}~n",
        [?NODEID, ?STAT, StatName, Type, Opts, Aliases])
  end.

re_register_stat(StatName, Type, Opts, Aliases) -> % returns -> ok.
  put(?STATKEY(StatName), StatName, {Type, Opts, Aliases}).

%% @doc
%% The Finds the option for status in the metaopts, for first time registration
%% should return false, in which case the options given are returned.
%% else the Status from the metadata takes precedent and is returned ontop of the
%% opts given
%% @end
find_status(MetaOpts) ->
  case lists:keyfind(status, 1, MetaOpts) of
    false ->
      [];
    Status -> % {status, disabled}
      Status
  end.

%%%%%%%%%% UNREGISTERING %%%%%%%%%%%%

-spec(unregister(StatName :: term()) -> ok | term()).
%% @doc
%% Marks the stats as unregistered, that way when a node is restarted and registers the
%% stats it will ignore stats that are marked unregistered
%% @end
unregister(Statname) ->
  case check_meta(?STATPFX(Statname)) of
    [] -> ok;
    unregistered -> ok;
    {Type, MetaOpts, Aliases} ->
      set_options({Statname, Type, MetaOpts, Aliases}, {status, unregistered});
    _ -> ok
  end.

%%%%%%%%% RESETTING %%%%%%%%%%%

-spec(reset_stat(Statname :: metadata_key()) -> ok | term()).
%% @doc
%% reset the stat in exometer and notify exometer of its reset
%% @end
reset_stat(Statname) ->
  case check_meta(?STATKEY(Statname)) of
    [] -> ok;
    unregistered -> {error, unregistered};
    {Type, Opts, Aliases} ->
      {value, {resets, Resets}} = lists:keysearch(resets, 1, Opts),
      Options = [{status, enabled}, {resets, reset_inc(Resets)}],
      set_options({Statname, Type, Opts, Aliases}, Options),
      ok
  end.

reset_inc(Count) -> Count + 1.


%%%===================================================================
%%% Profile API
%%%===================================================================

-spec(get_current_stats() -> term()).
%% @doc
%% retrieving the stats out of the metadata isn't as easy as just giving
%% the prefix, the stats names are stored in riak_stat_admin in an ets
%% table upon registration.
%% All the stats are pulled out of the ets table and returned here in
%% order to find the stats that are registered/enabled/disabled.
%% @end
get_current_stats() ->
  Stats = riak_stat_admin:get_stats(),
  lists:foldl(fun(Stat, Acc) ->
    [check_status(Stat) | Acc]
              end, [], Stats).



-spec(save_profile(ProfileName :: atom()) ->
  term() | {error, Reason :: term()}).
%% @doc
%% Take the stats and their status out of the metadata for the current
%% node and save it into the metadata as a profile
%% @end
save_profile(ProfileName) ->
  Stats = get_current_stats(),
  register_profile(ProfileName, Stats).

register_profile(ProfileName, Stats) ->
  case check_meta(?PROFILEKEY(ProfileName)) of
    [] ->
      put(?PROFPFX, ProfileName, Stats);
    _ ->
      {error, profile_exists_already}
  end.



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

get_profile_stats(ProfileName) ->
  case check_meta(?PROFILEKEY(ProfileName)) of
    [] ->
      {error, no_stats};
    Stats ->
      Stats
  end.

compare_stats(CurrentMetaStats, ProfileStats) ->
  the_alpha_stat(ProfileStats, CurrentMetaStats).

the_alpha_stat(Alpha, Beta) ->
  Raw =riak_stat_admin:the_alpha_stat(Alpha, Beta),
  reverse(Raw).

reverse(RawData) ->
  lists:map(fun
              ({Stat, {atom, {status, Status}}}) -> {Stat, {status, Status}};
              ({Stat, {atom, Val}}) -> {Stat, Val};
              ({Stat, {Atom, Val}}) -> {Stat, {Atom, Val}}
              end, RawData).

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

delete_p(Profile) ->
  delete({?PROFID, ?PROF}, Profile).



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

find_stats(Stats, Status) ->
  lists:foldl(
    fun({Stat, St}, Acc) when St == Status ->
      [{Stat, St} | Acc];
      ({_Stat, St}, Acc) when St =/= Status ->
        Acc
    end, [], Stats).
