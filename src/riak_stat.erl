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
-export([show_stat_status/2, show_stat_0/1, show_stat_info/1,
  disable_stat_0/1,
  change_stat_status/2]).

%% Admin API
-export([]).

%% Profile API
-export([]).

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
  app_helper:get_env(riak_core, stat_prefix, riak).

%%%===================================================================
%%% Console API
%%%===================================================================


show_stat_status(Arg, Status) ->
  riak_stat_console:show_stat(Arg, Status).

show_stat_0(Arg) ->
  riak_stat_console:show_stat_0(Arg). % todo: change to show_static_stats

disable_stat_0(Arg) ->
  riak_stat_console:disable_stat_0(Arg). % Todo: change to show disabled_stats

show_stat_info(Arg) ->
  riak_stat_console:stat_info(Arg).

show_static_stats(Arg) ->
  ok.

show_disabled_stats(Arg) ->
  ok.

change_stat_status(Arg, ToStatus) ->
  riak_stat_console:status_change(Arg, ToStatus).

reset_stat(Arg) ->
  riak_stat_console(Arg).


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

unregister(Stat) ->
  ok.




%%%===================================================================
%%% Profile API
%%%===================================================================

load_profile(Arg) ->
  ok.

save_current_profile(Arg) ->
  ok.

delete_profile(Arg) ->
  ok.

reset_stats_and_profile(Arg) ->
  ok.














