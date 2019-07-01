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

%% API
-export([
  prefix/0,
  register/2,
  get_app_stats/1, get_value/1, get_values/1,
  update/3,
  unregister/4]).

%% Init Internal Functions

prefix() ->
  app_helper:get_env(riak_core, stat_prefix, riak).

%% Registering Stats

register(App, Stats) ->
  riak_stat_admin:register(prefix(), App, Stats).

%% Reading stats and stat data

get_app_stats(App) ->
  riak_stat_exometer:read_stats(App).

get_value(Stat) ->
  riak_stat_exometer:get_value(Stat).

get_values(Path) ->
  riak_stat_exometer:get_values(Path).

%% Update the stats in Exometere

update(Name, IncrBy, Type) ->
  riak_stat_exometer:update_or_create(Name, IncrBy, Type).

%% Unregistering

unregister(Mod, Idx, Type, App) ->
  riak_stat_admin:unregister(prefix(), App ,Mod, Idx, Type).










%% Read

get_stat(Stat) ->
  ok.


get_stat_status(Stat) ->
  ok.


%% Update



set_stat_options(Stat, Options) ->
  ok.

set_options(Type, Options) ->
  ok.

reset_stat(Stat) ->
  ok.

%% delete

delete_stat(Stat) ->
  ok.

unregister(Stat) ->
  ok.

