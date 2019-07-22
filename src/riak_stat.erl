%%%-------------------------------------------------------------------
%%% @doc
%%% Top module of the riak_stat app.
%%% Most of the _stat modules in riak will call directly into this module
%%% to perform specific requests.
%%%
%%% This module calls into riak_stat_admin, riak_stat_console and
%%% riak_stat_profile depending on what is needed
%%% @end
%%%-------------------------------------------------------------------
-module(riak_stat).

-include("riak_stat.hrl").

%% Console API
-export([
    show_stat_status/2,
    show_static_stats/1,
    disable_stat_0/1,
    show_stat_info/1,
    change_stat_status/2,
    reset_stat/1
]).

%% Profile API
-export([
    save_current_profile/1,
    load_profile/1,
    delete_profile/1,
    reset_stats_and_profile/0
]).

%% Admin API
-export([
    prefix/0,
    register/2,
    get_stats/0,
    get_app_stats/1,
    get_stats_status/1,
    get_stats_info/1,
    get_value/1,
    aggregate/2,
    update/3,
    unregister/4,
    unregister/1
]).

%% Other riak_core_console API
-export([
    enable_metadata/1,
    data_persist/2
]).

-define(STAT_PREFIX, stat_prefix).

%%%===================================================================
%%% Console API
%%%===================================================================
%%%-------------------------------------------------------------------
%%% @doc
%%% Functions that are called from riak_core_console that are specific
%%% for the "riak-admin stat ..." commands.
%%% Are sent to the riak_stat_console module depending on the cmd.
%%% @end
%%%-------------------------------------------------------------------

-spec(show_stat_status(data(), status()) -> ok | print()).
%% @doc
%% A call of riak-admin stat show-enabled | show-disabled <entry>
%% points to this function, it will by default go to metadata unless it
%% is changed to exometer with set_default(Def)...
%% It just returns a status or statuses of the entry or entries
%% provided.
%% @end
show_stat_status(Arg, Status) ->
    riak_stat_console:show_stat(Arg, Status).

-spec(show_static_stats(data()) -> ok | print()).
%% @doc
%% A call from riak_core_console made through riak-admin of:
%% riak-admin stat show-0 <entry> -> which returns the stats in
%% exometer which have a value that is not updating.
%% It will show which stats that are not being used.
%%
%% %% in exometer if it is not updating it will have a value of 0
%% or []
%% @end
show_static_stats(Arg) ->
    riak_stat_console:show_stat_0(Arg).

-spec(disable_stat_0(data()) -> ok | print()).
%% @doc
%% like the function above it will find the stats in exometer that are not
%% updating but will in turn disable them in both the metadata and in
%% exometer so the change is persisted
%% @end
disable_stat_0(Arg) ->
    riak_stat_console:disable_stat_0(Arg).

-spec(show_stat_info(data()) -> ok | print()).
%% @doc
%% show the information that is kept in the metadata and in exometer for the stats
%% given
%% If the default is not metadata it will just return the information stored in
%% exometer
%% @end
show_stat_info(Arg) ->
    riak_stat_console:stat_info(Arg).

-spec(change_stat_status(data(), status()) -> ok | print()).
%% @doc
%% change the status of the stat in metadata and exometer, unless the default
%% is not metadata and is set to exometer then the data goes to exometer
%% only.
%% @end
change_stat_status(Arg, ToStatus) ->
    riak_stat_console:status_change(Arg, ToStatus).

-spec(reset_stat(data()) -> ok | term()).
%% @doc
%% reset the stat in the metadata and in exometer, both the metadata
%% and exometer keep history of the number of resets, except with the
%% metadata the number of resets is persisted
%% @end
reset_stat(Arg) ->
    riak_stat_console:reset_stat(Arg).


%%%===================================================================
%%% Profile API
%%%===================================================================
%%%-------------------------------------------------------------------
%%% @doc
%%% Specific functions for profiles in riak_stat, a layer of
%%% functionality to save the current status of all stats to reload
%%% later - allowing consistency in testing and laziness in stats.
%%% @end
%%%-------------------------------------------------------------------

-spec(save_current_profile(data()) -> ok | error()).
%% @doc
%% Pull out the current stats status, and store the profile_name and
%% list of stats into the metadata.
%% All unregistered stats are stored as {status, unregistered}, but kept
%% within the profile in case it becomes re_registered, it is defaulted to
%% disabled.
%% @end
save_current_profile(Arg) ->
    riak_stat_profiles:save_profile(Arg).

-spec(load_profile(data()) -> ok | error()).
%% @doc
%% load a profile saved in the metadata, if the profile does not exist then
%% {error, no_profile} is returned.
%% @end
load_profile(Arg) ->
    riak_stat_profiles:load_profile(Arg).

-spec(delete_profile(data()) -> ok | error()).
%% @doc
%% Deletes the profile in the metadata but does not reset the stats, that can
%% be done manually with reset-profile, this just removes the
%% snapshot of the stats from the metadata
%% @end
delete_profile(Arg) ->
    riak_stat_profiles:delete_profile(Arg).

-spec(reset_stats_and_profile() -> ok | error()).
%% @doc
%% Resets all disabled stats back to enabled. If the stat has a status of
%% {status, unregistered} then the stat is left as unregistered or disabled.
%% if it has been enabled since the profile was loaded then it will not be affected.
%% @end
reset_stats_and_profile() ->
    riak_stat_profiles:reset_profile().


%%%===================================================================
%%% Admin API
%%%===================================================================
%%%-------------------------------------------------------------------
%%% @doc
%%% Functions from _stat modules or from to register, update, read
%%% or unregister a stat.
%%%
%%% get_app_stats(App) -> calls into riak_stat_admin which finds the
%%% stats specifically for that app.
%%%
%%% get_stats_status(App) -> calls into riak_stat_admin to find the stats
%%% depending upon the priority set it will call into metadata (default)
%%% or exometer to find the status of all the stats for that specific
%%% App
%%%
%%% get_stats_info(App) -> calls into riak_stat_admin which pulls all
%%% the stats for that app out and then retrieves all the information
%%% stored in exometer for that stat
%%% @end
%%%-------------------------------------------------------------------

-spec(prefix() -> value()).
%% @doc
%% standard prefix for all stats inside riak, all modules call
%% into this function for the prefix.
%% @end
prefix() ->
    riak_stat_config:get_env(?STAT_PREFIX, riak).

-spec(register(app(), stats()) -> ok).
%% @doc
%% register the stats stored in the _stat modules in both the metadata
%% and in exometer_core
%% @end
register(App, Stats) ->
    riak_stat_admin:register(prefix(), App, Stats).

-spec(get_app_stats(app()) -> ok | stats()).
%% @doc
%% pulls the list of stats out of riak_stat_admin for that app.
%% @end
       get_app_stats(App) ->
    riak_stat_admin:read_stats(App).

-spec(get_stats_status(app()) -> stats()).
%% @doc
%% Pull the list of stats and their status out of riak_core_metadata
%% @end
get_stats_status(App) ->
    riak_stat_admin:get_stats_status(App).

-spec(get_stats_info(app()) -> stats()).
%% @doc
%% Pull the list of stats and their info from exometer
%% @end
get_stats_info(App) ->
    riak_stat_admin:get_stats_info(App).

-spec(get_value(data()) -> value()).
get_value(Arg) ->
    riak_stat_admin:get_stat_value(Arg).

-spec(aggregate(app(), stats()) -> value()).
%% @doc
%% exometer has the functionality to aggregate the stats.
%% @end
aggregate(App, Stats) ->
    riak_stat_admin:aggregate(App, Stats).

-spec(update(stat(), non_neg_integer(), type()) -> ok | arg()).
update(Name, IncrBy, Type) ->
    riak_stat_admin:update(Name, IncrBy, Type).

-spec(unregister(data()) -> ok | print()).
%% @doc
%% unregistering a stat puts tombstone in the metadata (if enabled)
%% deletes the metric in exometer
%% @end
unregister({Mod, Idx, Type, App}) ->
    unregister(Mod, Idx, Type, App).

unregister(Mod, Idx, Type, App) ->
    riak_stat_admin:unregister(prefix(), App ,Mod, Idx, Type).


%%%===================================================================
%%% Other API
%%%===================================================================
%%%-------------------------------------------------------------------
%%% @doc
%%% API that is called from riak_core_console specific for setting
%%% options in the config or retrieving stats
%%% @end
%%%-------------------------------------------------------------------

-spec(get_stats() -> stats()).
%% @doc
%% Pulls all the stats out of metadata or exometer if metadata is disabled
%% @end
get_stats() ->
    riak_stat_coordinator:get_stats().

-spec(enable_metadata(data()) -> ok).
%% @doc
%% enabling the metadata allows the stats configuration and the stats values to
%% be persisted, disabling the metadata returns riak to its original functionality
%% of only using the exometer functions. Enabling and disabling the metadata occurs
%% here, directing the stats and function work occurs in the riak_stat_coordinator
%% @end
enable_metadata(Arg) ->
    Truth = ?IS_ENABLED(?META_ENABLED),
    case riak_stat_admin:data_sanitise(Arg) of
        Truth ->
            case Truth of
                true ->
                    io:fwrite("Metadata already enabled~n");
                false ->
                    io:fwrite("Metadata already disabled~n")
            end;
        Bool when Bool == true; Bool == false ->
            set_env(?META_ENABLED, Bool);
        _ ->
            io:fwrite("Wrong argument entered: ~p~n", [_])
    end.

-spec(data_persist(datapersist(), data()) -> ok | error()).
%% @doc
%% Data can be pulled out of exometer and stored into the metadata in
%% order to be put into a DETS. It takes the argument Arg the same as
%% any of the other console commands, "riak.riak_kv.**" etc...
%%
%% for data_persist(profile, Arg) the Arg will be the name of a profile
%% stored in the metadata, specific to data persistence ot not.
%% @end
data_persist(enabled, Arg) ->
    riak_stat_admin:persist_data(Arg);
data_persist(disabled, Arg) ->
    riak_stat_admin:unpersist_data(Arg);
data_persist(profile, Arg) ->
    riak_stat_profiles:persist_profile_data(Arg).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

set_env(Arg, Value) ->
    riak_stat_config:set_env(Arg, Value).