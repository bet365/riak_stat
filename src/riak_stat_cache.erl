%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2019 14:41
%%%-------------------------------------------------------------------
-module(riak_stat_cache).

%% set env for caching
%% make riak_stat_console got through this first for gets


%% API
-export([check_cache/2, write_cache/3, delete_cache/2]).

-define(TTL, 300000). % 5min

check_cache(Name, DP) ->
  riak_stat_coordinator:read_cache(Name, DP).

write_cache(Name, DP, Val) ->
  riak_stat_coordinator:write_cache(Name, DP, Val, ?TTL).

delete_cache(Name, DP) ->
  riak_stat_coordinator:delete_cache(Name, DP).