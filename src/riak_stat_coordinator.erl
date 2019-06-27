%%%-------------------------------------------------------------------
%%% @author savannahallsop
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2019 14:41
%%%-------------------------------------------------------------------
-module(riak_stat_coordinator).
-author("savannahallsop").

%% API
-export([coordinate/2]).

%% console API
-export([]).

%% profiles API
-export([]).

%% admin API
-export([]).

-spec(coordinate(Arg :: term(), Type :: {atom(), atom()}) ->
  ok | term() | {error, Reason :: term()}).
%% @doc
%% Any data from the modules are sent to this function and then sent to the
%% necessary module to perform the function needed.
%% @end
coordinate(WhereTo, Arg) ->
  case WhereTo of
    metadata ->
      riak_stat_metadata:coordinate(Arg);
    exometer ->
      riak_stat_exometer:coordinate(Arg);
    admin ->
      riak_stat_admin:coordinate(Arg)
  end.


