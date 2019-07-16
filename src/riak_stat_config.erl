%%%-------------------------------------------------------------------
%%% @doc
%%% Wrapper for Application functions specific for riak_stat
%%% @end
%%%-------------------------------------------------------------------
-module(riak_stat_config).

-define(APP, riak_stat).

-export([get_env/1, get_env/2, set_env/2,
  ensure_all/0, get_all_info/0, get_all/0]).

get_env(Env) ->
  {ok, P} = application:get_env(?APP, Env),
  P.

get_env(Env, Default) ->
  {ok, P} = application:get_env(?APP, Env, Default),
  P.

set_env(Env, Value) ->
  application:set_env(?APP, Env, Value).

%% ---------------------------------------------------------------------

ensure_all() ->
  application:ensure_all_started(?APP).

get_all_info() ->
  application:get_all_env(?APP).

get_all() ->
  application:get_all_key(?APP).