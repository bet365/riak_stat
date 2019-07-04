%%%-------------------------------------------------------------------
%%% @author savannahallsop
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Jul 2019 14:54
%%%-------------------------------------------------------------------
-module(riak_stat).
-author("savannahallsop").

%% @doc
%% Core module of the riak_stat app, all functions from the
%% riak_core_console and stat modules use these functions to perform
%% the specific stats request needed.
%%
%% This module calls into riak_stat_admin, riak_stat_console and
%% riak_stat_profile depending on what is needed
%% @end

%% Console API
-export([
  show_stat_status/2, show_static_stats/1, show_stat_info/1,
  disable_stat_0/1, reset_stat/1,
  change_stat_status/2]).

%% Admin API
-export([clean_data/2]).

%% Profile API
-export([
  save_current_profile/1,
  load_profile/1,
  delete_profile/1,
  reset_stats_and_profile/0]).

%% Externally needed API
-export([prefix/0]).


%% API
-export([
  register/2,
  get_app_stats/1, get_value/1, get_values/1,
  update/3,
  unregister/4]).


%%%===================================================================
%%% Common API
%%%===================================================================

prefix() ->
  app_helper:get_env(riak_stat, stat_prefix, riak).

%%%===================================================================
%%% Console API
%%%===================================================================

-spec(show_stat_status(Arg :: term(), Status :: atom()) ->
  term()).
%% @doc
%% A call of riak-admin stat show-enabled | show-disabled <entry>
%% points to this function, it will by default go to metadata unless it
%% is changed to exometer with set_default(Def)...
%% It just returns a status or statuses of the entry or entries
%% provided.
%% @end
show_stat_status(Arg, Status) ->
  riak_stat_console:show_stat(Arg, Status).

-spec(show_static_stats(Arg :: term()) -> term()).
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
  riak_stat_console:show_stat_0(Arg). % todo: change to show_static_stats

-spec(disable_stat_0(Arg :: term()) -> term()).
%% @doc
%% like the function above it will find the stats in exometer that are not
%% updating but will in turn disable them in both the metadata and in
%% exometer so the change is persisted
%% @end
disable_stat_0(Arg) ->
  riak_stat_console:disable_stat_0(Arg). % Todo: change to show disabled_stats

-spec(show_stat_info(Arg :: term()) -> term()).
%% @doc
%% show the information that is kept in the metadata and in exometer for the stats
%% given
%% If the default is not metadata it will just return the information stored in
%% exometer
%% @end
show_stat_info(Arg) ->
  riak_stat_console:stat_info(Arg).

-spec(change_stat_status(Arg :: term(), ToStatus :: atom()) -> term()).
%% @doc
%% change the status of the stat in metadata and exometer, unless the default
%% is not metadata and is set to exometer then the data goes to exometer
%% only.
%% @end
change_stat_status(Arg, ToStatus) ->
  riak_stat_console:status_change(Arg, ToStatus).

-spec(reset_stat(Arg :: term()) -> term()).
%% @doc
%% reset the stat in the metadata and in exometer, both the metadata
%% and exometer keep history of the number of resets, except with the
%% metadata the number of resets is persisted
%% @end
reset_stat(Arg) ->
  riak_stat_console:reset_stat(Arg).


%%%===================================================================
%%% Admin API
%%%===================================================================

register(App, Stats) ->
  riak_stat_admin:register(prefix(), App, Stats).

%% Reading stats and stat data

get_app_stats(App) ->
  riak_stat_exometer:read_stats(App). % TODO: point to admin

get_value(Stat) ->
  riak_stat_exometer:get_value(Stat). % point to admin

get_values(Path) ->
  riak_stat_exometer:get_values(Path). % admin

%% Update the stats in Exometere

update(Name, IncrBy, Type) -> % point to admin
  riak_stat_exometer:update_or_create(Name, IncrBy, Type).

%% Unregistering

unregister({Mod, Idx, Type, App}) ->
  unregister(Mod, Idx, Type, App).

unregister(Mod, Idx, Type, App) ->
  riak_stat_admin:unregister(prefix(), App ,Mod, Idx, Type).


% Read

get_stat(Stat) ->
  ok.


get_stat_status(Stat) ->
  ok.


%% Update



set_stat_options(Stat, Options) ->
  ok.

set_options(Type, Options) ->
  ok.

%% delete

delete_stat(Stat) ->
  ok.



%%%===================================================================
%%% Profile API
%%%===================================================================

-spec(save_current_profile(Arg :: term()) -> ok | term()).
%% @doc
%% Pull out the current stats status, and store the profile_name and
%% list of stats into the metadata.
%% All unregistered stats are stored as {status, unregistered}, but kept
%% within the profile in case it becomes re_registered, it is defaulted to
%% disabled.
%% @end
save_current_profile(Arg) ->
  CleanArg = clean_profile_name(Arg),
  riak_stat_profiles:save_profile(CleanArg).

-spec(load_profile(Arg :: term()) -> ok | term() | {error, Reason :: term()}).
%% @doc
%% load a profile saved in the metadata, if the profile does not exist then
%% {error, no_profile} is returned.
%% @end
load_profile(Arg) ->
  CleanArg = clean_profile_name(Arg),
  riak_stat_profiles:load_profile(CleanArg).

-spec(delete_profile(Arg :: term()) -> ok | term() | {error, Reason :: term()}).
%% @doc
%% Deletes the profile in the metadata but does not reset the stats, that can
%% be done manually with the function reset_profile, this just removes the
%% snapshot of the stats from the metadata
%% @end
delete_profile(Arg) ->
  CleanArg = clean_profile_name(Arg),
  riak_stat_profiles:delete_profile(CleanArg).

-spec(reset_stats_and_profile() -> ok | {error, Reason :: term()}).
%% @doc
%% Resets all disabled stats back to enabled. If the stat has a status of
%% {status, unregistered} then the stat is left as unregistered or disabled.
%% if it has been enabled since the profile was loaded then it will not be affected.
%% @end
reset_stats_and_profile() ->
  riak_stat_profiles:reset_profile().













