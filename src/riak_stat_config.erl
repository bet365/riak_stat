%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(riak_stat_config).

-include("exoskeleskin.hrl").

%% TODO

% more config related functions

%% API
-export([get_env/1, get_env/2, set_env/2]).

get_env(Env) ->
  {ok, P} = application:get_env(?APP, Env),
  P.

get_env(Env, Default) ->
  {ok, P} = application:get_env(?APP, Env, Default),
  P.

set_env(Env, Value) ->
  application:set_env(?APP, Env, Value).
