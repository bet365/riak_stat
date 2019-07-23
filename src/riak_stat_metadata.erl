%%%-------------------------------------------------------------------
%%% @doc
%%% riak_stat_metadata is the middle-man for riak_stat and
%%% riak_core_metadata. All information that needs to go into or out
%%% of the metadata will always go through this module.
%%% @end
%%%-------------------------------------------------------------------
-module(riak_stat_metadata).

-include_lib("riak_core/include/riak_core_metadata.hrl").
-include("riak_stat.hrl").

%% Basic API
-export([
    get/2,
    get/3,
    put/3,
    put/4,
    get_all/1,
    delete/2,
    select/2,
    replace/2
]).

%% API
-export([
    check_meta/1,
    check_status/1,
    change_status/1,
    change_status/2,
    set_options/2
]).

%% Admin API
-export([
    register_stat/4,
    register_stat/1,
    unregister/1,
    reset_stat/1,
    reset_resets/0,
    get_stats_status/0
]).

%% Profile API
-export([
    get_profiles/0,
    save_profile/1,
    load_profile/1,
    delete_profile/1,
    reset_profile/0,
    get_loaded_profile/0
]).

-define(PFX, riak_stat:prefix()).
-define(STAT, stats).
-define(PROF, profiles).
-define(NODEID, riak_core_nodeid:get()).
-define(PROFID, list).
-define(STATPFX,              {?STAT, ?NODEID}).
-define(PROFPFX,              {?PROF, ?PROFID}).
-define(STATKEY(StatName),    {?STATPFX, StatName}).
%%      Profiles are Globally shared
-define(PROFILEKEY(Profile),  {?PROFPFX, Profile}).
-define(LOADEDPFX,            {?PROF, loaded}).
-define(LOADEDKEY,             "profile-key").
-define(LOADEDPKEY,           {?LOADEDPFX, ?LOADEDKEY}).

%%%===================================================================
%%% Basic API
%%%===================================================================

-spec(get(metadata_prefix(), metadata_key()) -> metadata_value() | undefined).
%% @doc
%% Get the data from the riak_core_metadata, If not Opts are passed then an empty
%% list is given and the defaults are set in the riak_core_metadata.
%% it's possible to do a select pattern in the options under the form:
%%      {match, ets:match_spec}
%% Which is pulled out in riak_core_metadata and used in an ets:select,
%% @end
get(Prefix, Key) ->
    get(Prefix, Key, []).
get(Prefix, Key, Opts) ->
    riak_core_metadata:get(Prefix, Key, Opts).

-spec(put(metadata_prefix(), metadata_key(), metadata_value() | metadata_modifier(), put_opts()) -> ok).
%% @doc
%% put the data into the metadata, options contain the {match, Match_spec}
%% @end
put(Prefix, Key, Value) ->
    put(Prefix, Key, Value, []).
put(Prefix, Key, Value, Opts) ->
    riak_core_metadata:put(Prefix, Key, Value, Opts).

-spec(get_all(metadata_prefix()) -> metadata_value()).
%% @doc
%% Give a Prefix for anything in the metadata and get a list of all the
%% data stored under that prefix
%% @end
get_all(Prefix) ->
    riak_core_metadata:to_list(Prefix).

-spec(delete(metadata_prefix(), metadata_key()) -> ok).
%% @doc
%% deleting the key from the metadata replaces values with tombstone
%% @end
delete(Prefix, Key) ->
    riak_core_metadata:delete(Prefix, Key).

-spec(select(metadata_prefix(), pattern()) -> metadata_value()).
%% @doc
%% use the ets:select in the metadata to pull the value wanted from the metadata
%% using ets:select is more powerful that using the get function
%% @end
select(Prefix, MatchSpec) ->
    riak_core_metadata:select(Prefix, MatchSpec).

-spec(replace(metadata_prefix(), pattern()) -> metadata_value()).
%% @doc
%% pull the data out of the metadata and replace any values that need changing
%% i.e. any statuses that need changing can use the ets:select_replace function
%% that is the basis for this function
%% @end
replace(Prefix, MatchSpec) ->
    riak_core_metadata:replace(Prefix, MatchSpec).

%%-spec(ms(function()) -> pattern()).
%%%% @doc
%%%% turn a fun into a match spec
%%%% @end
%%ms(Fun) ->
%%    ets:fun2ms(Fun).
%%
%%-spec(get_put_ms(pattern()) -> {match, pattern()}).
%%%% @doc
%%%% puts the match_spec into a form usable in the riak_core_metadata
%%%% @end
%%get_put_ms(MatchSpec) ->
%%    {match, MatchSpec}.

%%%===================================================================
%%% Profile API
%%%===================================================================

-spec(save_profile(profilename()) -> ok | error()).
%% @doc
%% Take the stats and their status out of the metadata for the current
%% node and save it into the metadata as a profile - works on one node
%% @end
save_profile(ProfileName) ->
    StatsStatus = get_stats_status(),
    register_profile(ProfileName, StatsStatus).

register_profile(ProfileName, Stats) ->
    case check_meta(?PROFILEKEY(ProfileName)) of
        [] ->
            put(?PROFPFX, ProfileName, Stats);
        _ ->
            {error, profile_exists_already}
    end.


-spec(load_profile(profilename()) -> ok | error()).
%% @doc
%% Find the profile in the metadata and pull out stats to change them
%% It will compare the current stats with the profile stats and will
%% change the ones that need changing to prevent errors
%% @end
load_profile(ProfileName) ->
    case get_profile_stats(ProfileName) of
        {error, Reason} ->
            {error, Reason};
        ProfileStats ->
            CurrentStats = get_stats_status(),
            ToChange = the_alpha_stat(ProfileStats, CurrentStats),
            %% delete stats that are already enabled/disabled, any duplicates
            %% with different statuses will be replace with the profile one
            change_stat_list_to_status(ToChange),
            put(?LOADEDPFX, ?LOADEDKEY, ProfileName)
    end.

get_profile_stats(ProfileName) ->
    case check_meta(?PROFILEKEY(ProfileName)) of
        [] ->
            {error, no_stats};
        Stats ->
            Stats
    end.

the_alpha_stat(Alpha, Beta) ->
    riak_stat_admin:the_alpha_stat(Alpha, Beta).

change_stat_list_to_status(StatusList) ->
    riak_stat_coordinator:change_status(StatusList).


-spec(delete_profile(profilename()) -> ok).
%% @doc
%% Deletes the profile from the metadata, however currently the metadata
%% returns a tombstone for the profile, it can be overwritten when a new profile
%% is made of the same name, and in the profile gen_server the name of the
%% profile is "unregistered" so it can not be reloaded again after deletion
%% @end
delete_profile(ProfileName) ->
    case check_meta(?LOADEDPKEY) of
        ProfileName ->
            put(?LOADEDPFX, ?LOADEDKEY, [<<>>]),
            delete(?PROFPFX, ProfileName);
        _ ->
            delete(?PROFPFX, ProfileName)
    end.


-spec(reset_profile() -> ok | error()).
%% @doc
%% resets the profile by enabling all the stats, pulling out all the stats that
%% are disabled in the metadata and then changing them to enabled in both the
%% metadata and exometer
%% @end
reset_profile() ->
    CurrentStats = get_stats_status(),
    change_stats_from(CurrentStats, disabled).
    % change from disabled to enabled

change_stats_from(Stats, Status) ->
    ToChange =
        lists:foldl(fun
                        ({Stat, {status, St}}, Acc) when St == Status ->
                            NewSt =
                                case Status of
                                    enabled -> disabled;
                                    disabled -> enabled
                                end,
                            [{Stat, {status, NewSt}} | Acc];
                        ({_Stat, {status, St}}, Acc) when St =/= Status ->
                            Acc
                    end, [], Stats),
    change_stat_list_to_status(ToChange).


-spec(get_profiles() -> metadata_value()).
%% @doc
%% returns a list of the profile names stored in the metadata
%% @end
get_profiles() ->
    get_all(?PROFPFX).

-spec(get_loaded_profile() -> profilename()).
%% @doc
%% get the profile that is loaded in the metadata
%% @end
get_loaded_profile() ->
    get(?LOADEDPFX, ?LOADEDKEY).


%%%===================================================================
%%% API
%%%===================================================================

-spec(check_meta(metadata_pkey()) -> metadata_value()).
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
                _            -> Value
            end
    end.

find_unregister_status(_K, '$deleted') ->
    unregistered;
find_unregister_status(_SN, {Status, _T, _Opts, _A}) ->
    Status; % enabled | disabled =/= unregistered
find_unregister_status(_PN, _Stats) ->
    false.

%%%%%%%%%% READING OPTS %%%%%%%%%%%%

-spec(check_status(metadata_key()) -> metadata_value() | error()).
%% @doc
%% Returns the status of the stat saved in the metadata
%% @end
check_status(StatName) ->
    case check_meta(?STATPFX(StatName)) of
        {Status, _Type, _Opts, _Aliases} ->
            {StatName, {status, Status}};
        _ ->
            {error, no_stat}
    end.

-spec(change_status(metadata_key(), status()) -> ok | acc()).
%% @doc
%% Changes the status in the metadata
%% @end
change_status(Stats) when is_list(Stats) ->
    lists:foldl(fun
                ({Stat, {status, Status}}, Acc) ->
                    [change_status(Stat, Status) | Acc];
                ({Stat, Status}, Acc) ->
                    [change_status(Stat, Status) | Acc]
              end, [], Stats);
change_status({StatName, Status}) ->
    change_status(StatName, Status).
change_status(Statname, ToStatus) ->
    case check_meta(?STATKEY(Statname)) of
      [] ->
          [];
      unregistered ->
          [];
      {_Status, Type, Opts, Aliases} ->
          put(?STATPFX, Statname, {ToStatus, Type, Opts, Aliases})
    end.

%%%%%%%%%% SET OPTIONS %%%%%%%%%%%%%

-spec(set_options(metadata_key(), options()) -> ok).
%% @doc
%% Setting the options in the metadata manually, such as
%% resets etc...
%% @end
set_options(StatInfo, NewOpts) when is_list(NewOpts) ->
    lists:foreach(fun({Key, NewVal}) ->
        set_options(StatInfo, {Key, NewVal})
                  end, NewOpts);
set_options({Statname, {Status, Type, Opts, Aliases}}, {Key, NewVal}) ->
    NewOpts = lists:keyreplace(Key, 1, Opts, {Key, NewVal}),
    NewOpts2 = fresh_clock(NewOpts),
    set_options(Statname, {Status, Type, NewOpts2, Aliases});
set_options(StatName, {Status, Type, NewOpts, Aliases}) ->
    re_register_stat(StatName, {Status, Type, NewOpts, Aliases}).

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

%%get_stats_from_path(Path) ->
%%    select(?STATPFX, [{{Path, {'_','_','_','_'}},[],['$_']}]).

-spec(get_stats_status() -> metadata_value()).
%% @doc
%% retrieving the stats out of the metadata and their status
%% @end
get_stats_status() ->
    select(?STATPFX, stats_status_ms()).

stats_status_ms() -> %% for every stat tho
    ets:fun2ms(fun({StatName, {Status, '_', '_', '_'}}) -> {StatName, {status, Status}} end).


%%%%%%%%%%%% REGISTERING %%%%%%%%%%%%

register_stat({StatName, Type, Opts, Aliases}) ->
    register_stat(StatName, Type, Opts, Aliases).
-spec(register_stat(metadata_key(), type(), options(), aliases()) -> ok | options()).
%% @doc
%% Checks if the stat is already registered in the metadata, if not it
%% registers it, and pulls out the options for the status and sends it
%% back to go into exometer
%% @end
register_stat(StatName, Type, Opts, Aliases) ->
    case check_meta(?STATPFX(StatName)) of % check registration
        [] -> % if not registered return default Opts
            {Status, MetaOpts} = find_status(fresh, Opts),
            re_register_stat(StatName, {Status, Type, [{vclock, vclock:fresh(?NODEID, 1)} | MetaOpts], Aliases}),
            Opts;
        unregistered -> [];
    %%      {error, unregistered};
        {MStatus, Type, MetaOpts, Aliases} -> % if registered
            {Status, NewMetaOptions, NewOpts} = find_status(re_reg, {Opts, MStatus, MetaOpts}),
            re_register_stat(StatName, {Status, Type, NewMetaOptions, Aliases}),
            NewOpts;
        _ ->
            lager:debug("riak_stat_meta_mgr:register_stat --
            Could not register the stat:~n{{~p,~p},~p,{~p,~p,~p}}~n",
              [?NODEID, ?STAT, StatName, Type, Opts, Aliases])
    end.

re_register_stat(StatName, StatValue) -> % returns -> ok.
  put(?STATPFX, StatName, StatValue).

%% @doc
%% The Finds the option for status in the metaopts, for first time registration
%% should return false, in which case the options given are returned.
%% else the Status from the metadata takes precedent and is returned ontop of the
%% opts given
%% @end
find_status(fresh, Opts) ->
    case proplists:get_value(status, Opts) of
        undefined -> {enabled, Opts}; % default is enabled, with original opts in
        Status    -> {Status, lists:keydelete(status, 1, Opts)} % set status and add options w/o status
    end;
find_status(re_reg, {Opts, MStatus, MOpts}) ->
    case proplists:get_value(status, Opts) of
        undefined ->
            {MStatus, the_alpha_stat(MOpts, Opts), [{status, MStatus} | Opts]};
        _Status    ->
            {MStatus, the_alpha_stat(MOpts, Opts), lists:keyreplace(status, 1, Opts, {status, MStatus})}
    end.

%%%%%%%%%% UNREGISTERING %%%%%%%%%%%%

-spec(unregister(metadata_key()) -> ok).
%% @doc
%% Marks the stats as unregistered, that way when a node is restarted and registers the
%% stats it will ignore stats that are marked unregistered
%% @end
unregister(Statname) ->
    case check_meta(?STATPFX(Statname)) of
        {_Status, Type, MetaOpts, Aliases} ->
            re_register_stat(Statname, {unregistered, Type, MetaOpts, Aliases});
        _ -> ok
    end.

%%%%%%%%% RESETTING %%%%%%%%%%%

-spec(reset_stat(metadata_key()) -> ok | error()).
%% @doc
%% reset the stat in exometer and notify metadata of its reset
%% @end
reset_stat(Statname) ->
    case check_meta(?STATKEY(Statname)) of
        [] -> ok;
        unregistered -> {error, unregistered};
        {_Status, Type, Opts, Aliases} ->
            Resets= proplists:get_value(resets, Opts),
            Options = [{resets, reset_inc(Resets)}],
            set_options({Statname, {enabled, Type, Opts, Aliases}}, Options)
    end.

reset_inc(Count) -> Count + 1.

-spec(reset_resets() -> ok).
%% @doc
%% sometimes the reset count just gets too high, and for every single
%% stat its a bit much
%% @end
reset_resets() ->
    lists:foreach(fun({Stat, _Val}) ->
        {Status, Type, Opts, Aliases} = check_meta(?STATKEY(Stat)),
        set_options({Stat, {Status, Type, Opts, Aliases}}, {resets, 0})
                  end, get_all(?STATPFX)).








%%%===================================================================
%%% Exoskeleskin API
%%%===================================================================


%%meta_select(Prefix, Pattern) when is_list(Pattern) ->
%%    try list_to_tuple(Pattern) of
%%        T -> meta_select(Prefix, T)
%%    catch
%%        error:_  ->
%%            lager:error("Pattern for metadata iterator wrong format ~n")
%%            NP = ets:fun2ms(fun({Stat, {Type, Opts, Aliases}}) when Prefix =:= ?STATPFX ->
%%                    {Stat, {Type, Opts, Aliases}}
%%                       end),
%%            meta_select(Prefix, NP)
%%    end;
%%meta_select(Prefix, Pattern) when is_tuple(Pattern) ->
%%    MatchSpec = {match, Pattern},
%%    riak_core_metadata:iterator(Prefix, MatchSpec).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% data persistence %%%
%%%%%%%%%%%%%%%%%%%%%%%%

%%-spec(persist_profile_data(profilename()) -> ok | error()).
%%%% @doc
%%%% stats that are enabled and saved in this profile are put into the metadata and
%%%% persisted, scraped every 10 seconds and saved into the metadata
%%%% @end
%%persist_profile_data(Profile) ->
%%    case get_profile_stats(Profile) of
%%        {error, Reason} ->
%%            {error, Reason};
%%        ProfileStats ->
%%            EnabledStats = find_stats(ProfileStats, enabled),
%%            StatsValues  = pull_from_exom(EnabledStats),
%%            lists:map(fun({Stat, Value}) ->
%%                put(?LOADEDPFX, Stat, Value)
%%                      end, StatsValues)
%%    end,
%%    ok.

%%pull_from_exom(Stats) ->
%%    riak_stat_coordinator:get_stats_values(Stats).