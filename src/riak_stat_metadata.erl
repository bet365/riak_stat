%%%-------------------------------------------------------------------
%%% @author savannahallsop
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2019 14:40
%%%-------------------------------------------------------------------
-module(riak_stat_metadata).
-author("savannahallsop").

%% Primary API
-export([get/2, put/4, delete/2]).

%% Secondary API
-export([]).

%% helper API
-export([]).

get(Prefix, Key) ->
  riak_core_metadata:get(Prefix, Key).

put(Prefix, Key, Value, Opts) ->
  riak_core_metadata:put(Prefix, Key, Value, Opts).

delete(Prefix, Key) ->
  riak_core_metadata:delete(Prefix, Key).






%% register_stats -> exometer
%% pull out the options and send to exometer, checking isnt really necessary

%% make Primary functions calls to the metadata

%% secondary calls, calls necessary for specific funstions or data

%% helper functions