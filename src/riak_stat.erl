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

%% Externally needed API
-export([
    prefix/0,
    set_priority/1,
    get_stats/0
]).

%% Admin API
-export([
    register/2,
    get_app_stats/1,
    get_stats_status/1,
    get_stats_info/1,
    get_value/1,
    aggregate/2,
    update/3,
    unregister/4,
    unregister/1
]).

%% Console API
-export([
    show_stat_status/2,
    show_static_stats/1,
    show_stat_info/1,
    disable_stat_0/1,
    reset_stat/1,
    change_stat_status/2
]).

%% Profile API
-export([
    save_current_profile/1,
    load_profile/1,
    delete_profile/1,
    reset_stats_and_profile/0
]).

%%%===================================================================
%%% Needed API
%%%===================================================================
%%%-------------------------------------------------------------------
%%% @doc
%%% API that is called from all or most of the other modules
%%% @end
%%%-------------------------------------------------------------------

-spec(prefix() -> value()).
%% @doc standard prefix for all stats inside riak, all modules call
%% into this function for the prefix.
%% @end
prefix() ->
    app_helper:get_env(riak_stat, stat_prefix, riak).

-spec(set_priority(arg()) -> ok).
%% @doc
%% Priority is an additional functionality that allows the metadata
%% to be used or bypassed, in case of an error etc.. can be set in riak
%% attach riak_stat:set_priority(metadata | exometer) or in the riak
%% client "riak-admin stat set-priority <entry>"
%% @end
set_priority(Priority) when is_atom(Priority) ->
    riak_stat_admin:set_priority(Priority);
set_priority(Priority) when is_list(Priority) ->
    set_priority(list_to_atom(Priority));
set_priority(Priority) when is_binary(Priority) ->
    set_priority(binary_to_existing_atom(Priority, latin1)).

-spec(get_stats() -> stats()).
%% @doc
%% Pulls the list of stats out of the riak_stat_admin ets table
%% @end
get_stats() ->
    riak_stat_admin:get_stats().


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

-spec(unregister(arg()) -> ok | print()).
unregister({Mod, Idx, Type, App}) ->
    unregister(Mod, Idx, Type, App).

unregister(Mod, Idx, Type, App) ->
    riak_stat_admin:unregister(prefix(), App ,Mod, Idx, Type).

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

-spec(show_stat_status(arg(), status()) -> ok | print()).
%% @doc
%% A call of riak-admin stat show-enabled | show-disabled <entry>
%% points to this function, it will by default go to metadata unless it
%% is changed to exometer with set_default(Def)...
%% It just returns a status or statuses of the entry or entries
%% provided.
%% @end
show_stat_status(Arg, Status) ->
    riak_stat_console:show_stat(Arg, Status).

-spec(show_static_stats(arg()) -> ok | print()).
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

-spec(show_stat_info(arg()) -> ok | print()).
%% @doc
%% show the information that is kept in the metadata and in exometer for the stats
%% given
%% If the default is not metadata it will just return the information stored in
%% exometer
%% @end
show_stat_info(Arg) ->
    riak_stat_console:stat_info(Arg).

-spec(disable_stat_0(arg()) -> ok | print()).
%% @doc
%% like the function above it will find the stats in exometer that are not
%% updating but will in turn disable them in both the metadata and in
%% exometer so the change is persisted
%% @end
disable_stat_0(Arg) ->
    riak_stat_console:disable_stat_0(Arg).

-spec(reset_stat(arg()) -> ok | term()).
%% @doc
%% reset the stat in the metadata and in exometer, both the metadata
%% and exometer keep history of the number of resets, except with the
%% metadata the number of resets is persisted
%% @end
reset_stat(Arg) ->
    riak_stat_console:reset_stat(Arg).

-spec(change_stat_status(arg(), status()) -> ok | print()).
%% @doc
%% change the status of the stat in metadata and exometer, unless the default
%% is not metadata and is set to exometer then the data goes to exometer
%% only.
%% @end
change_stat_status(Arg, ToStatus) ->
    riak_stat_console:status_change(Arg, ToStatus).

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

-spec(save_current_profile(arg()) -> ok | error()).
%% @doc
%% Pull out the current stats status, and store the profile_name and
%% list of stats into the metadata.
%% All unregistered stats are stored as {status, unregistered}, but kept
%% within the profile in case it becomes re_registered, it is defaulted to
%% disabled.
%% @end
save_current_profile(Arg) ->
    riak_stat_profiles:save_profile(Arg).

-spec(load_profile(arg()) -> ok | error()).
%% @doc
%% load a profile saved in the metadata, if the profile does not exist then
%% {error, no_profile} is returned.
%% @end
load_profile(Arg) ->
    riak_stat_profiles:load_profile(Arg).

-spec(delete_profile(arg()) -> ok | error()).
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