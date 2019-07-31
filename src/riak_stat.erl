%%%-------------------------------------------------------------------
%%% @doc
%%% Top module of the riak_stat app.
%%% Most of the _stat modules in riak will call directly into this module
%%% to perform specific requests through riak_admin.
%%% todo: Add more docs to the functions front wiki docs
%%% @end
%%%-------------------------------------------------------------------
-module(riak_stat).

-include("riak_stat.hrl").

%% Console API
-export([
    show_stat_status/1,
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
    get_stats_info/1,
    get_stats_values/1,
    get_value/1,
    aggregate/2,
    update/3,
    unregister/4,
    unregister/1
]).

%% Other riak_core_console API
-export([
    enable_metadata/1,
    enable_exoskeleskin/1,
    timestamp/0,
    stop/0,
    start/0,
    get_stat/1
]).

-define(STAT_PREFIX, stat_prefix).

%%%===================================================================
%%% Console API
%%%===================================================================
%%%-------------------------------------------------------------------
%%% @doc
%%% Functions that are called from riak_core_console that are specific
%%% for the "riak-admin stat ..." commands.
%%% Are sent to the riak_stat_console module
%%% @end
%%%-------------------------------------------------------------------

-spec(show_stat_status(data()) -> ok | print()).
%% @doc
%% A call of riak-admin stat show <entry>
%% points to this function, it will by default go to metadata unless it
%% is changed to exometer with set_default(Def)...
%% It just returns a status or statuses of the entry or entries
%% provided.
%% @end
show_stat_status(Arg) ->
    riak_stat_console:show_stat(Arg).

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
%% exometer
%% @end
disable_stat_0(Arg) ->
    riak_stat_console:disable_stat_0(Arg).

-spec(show_stat_info(data()) -> ok | print()).
%% @doc
%% show the information that is kept in exometer for the stats given
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
%% within the profile in case it becomes re_registered. If it is saved as
%% unregistered before it gets re_registered again, upon load of that
%% profile the stat will be ignored.
%% @end
save_current_profile(Arg) ->
    riak_stat_profiles:save_profile(Arg).

-spec(load_profile(data()) -> ok | error()).
%% @doc
%% load a profile saved in the metadata, if the profile does not exist then
%% {error, no_profile} is returned
%% The stats and their status (enabled | disabled | unregistered) are stored
%% in a list ([{StatName, {status, Status}}]) alongside the profile name,
%% when the profile is loaded the stats are compared to the current status
%% of stats and for any stats that are different; they are sent to exometer
%% to get enabled/disabled.
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

timestamp() -> ?TIMESTAMP.

-spec(prefix() -> value()).
%% @doc
%% standard prefix for all stats inside riak, all modules call
%% into this function for the prefix.
%% @end
prefix() ->
    riak_stat_config:get_env(?STAT_PREFIX, riak).


%%% ------------------------------------------------------------------

-spec(get_stats() -> stats()).
%% @doc
%% Pulls all the stats out of metadata or exometer if metadata is disabled
%% @end
get_stats() ->
    riak_stat_admin:get_stats().

%%% ------------------------------------------------------------------


-spec(get_stat(statname()) -> stats()).
%% @doc
%% get the stats from this path using ets:select
%% @end
get_stat(Path) ->
    riak_stat_admin:get_stat(Path).

-spec(get_value(data()) -> value()).
%% @doc
%% get the value of a stat
%% @end
get_value(Arg) ->
    riak_stat_admin:get_stat_value(Arg).


%%% ------------------------------------------------------------------

-spec(get_app_stats(app()) -> ok | stats()).
%% @doc
%% pulls the list of stats out of riak_stat_admin for that app.
%% @end
get_app_stats(App) ->
    riak_stat_admin:get_app_stats(App).

-spec(get_stats_values(app()) -> statlist()).
%% @doc
%% Get the stats from exometer and their values for that app.
%% @end
get_stats_values(App) ->
    riak_stat_admin:get_stats_values(App).

-spec(get_stats_info(app()) -> stats()).
%% @doc
%% Pull the list of stats and all their info from exometer
%% @end
get_stats_info(App) ->
    riak_stat_admin:get_stats_info(App).

%%% ------------------------------------------------------------------

-spec(register(app(), stats()) -> ok).
%% @doc
%% register the stats stored in the _stat modules in both the metadata
%% and in exometer_core
%% @end
register(App, Stats) ->
    riak_stat_admin:register(prefix(), App, Stats).

-spec(update(stat(), non_neg_integer(), type()) -> ok | arg()).
%% @doc
%% update the stat
%% @end
update(Name, IncrBy, Type) ->
    riak_stat_coordinator:update(Name, IncrBy, Type).

-spec(unregister(data()) -> ok | print()).
%% @doc
%% un-registering a stat puts a tombstone in the metadata (if enabled)
%% deletes the metric in exometer
%% @end
unregister({Mod, Idx, Type, App}) ->
    unregister(Mod, Idx, Type, App).

unregister(Mod, Idx, Type, App) ->
    riak_stat_admin:unregister(prefix(), App ,Mod, Idx, Type).

-spec(aggregate(app(), stats()) -> value()).
%% @doc
%% exometer has the functionality to aggregate the stats.
%% @end
%% TODO: apply functionality to aggregate stats from riak-admin commands
aggregate(Stats, DPs) ->
    riak_stat_admin:aggregate(Stats, DPs).


%%%===================================================================
%%% Other API
%%%===================================================================
%%%-------------------------------------------------------------------
%%% @doc
%%% API that is called from riak_core_console specific for setting
%%% options in the config or retrieving stats
%%% @end
%%%-------------------------------------------------------------------

-spec(enable_metadata(data()) -> ok).
%% @doc
%% enabling the metadata allows the stats configuration and the stats values to
%% be persisted, disabling the metadata returns riak to its original functionality
%% of only using the exometer functions. Enabling and disabling the metadata occurs
%% here, directing the stats and function work occurs in the riak_stat_coordinator
%% @end
enable_metadata(Arg) ->
    Truth = ?IS_ENABLED(?META_ENABLED),
    case data_sanitise(Arg) of
        Truth ->
            io:fwrite("Metadata-enabled already set to ~s~n", [Arg]);
        Bool when Bool == true; Bool == false ->
            case Bool of
                true ->
                    riak_stat_coordinator:reload_metadata(get_stats()),
                    set_env(?META_ENABLED, Bool);
                false ->
                    set_env(?META_ENABLED, Bool)
            end;
        _ ->
            io:fwrite("Wrong argument entered: ~p~n", [_])
    end.


-spec(enable_exoskeleskin(data()) -> ok).
%% @doc
%% The exoskeleskin sub-app is part of riak_stat, it starts up a gen_server for
%% http and udp requests to push stats to an endpoint, default is false, so the
%% exoskeleskin is disabled, but its status is also saved in the metadata
%% so it can be enabled when it is restarted as well
%% @end
enable_exoskeleskin(Arg) ->
    Truth = ?IS_ENABLED(?EXOSKEL_ENABLED),
    case data_sanitise(Arg) of
        Truth ->
            io:fwrite("Exoskeleskin already enabled~n");
        Bool when Bool == true, Bool == false ->
            set_env(?EXOSKEL_ENABLED, Bool);
        _ ->
            io:fwrite("Wrong Argument Entered~n")
    end.


%%%===================================================================
%%% Helper Functions
%%%===================================================================

set_env(Arg, Value) ->
    riak_stat_config:set_env(Arg, Value).

data_sanitise(Arg) ->
    {Data, _Other, _Stuff} = riak_stat_admin:data_sanitise(Arg),
    Data.

%%%%%%% TESTING %%%%%%%

start() ->
    riak_stat_exometer:start().

stop() ->
    riak_stat_exometer:stop().
