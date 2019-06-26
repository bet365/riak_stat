%%%-------------------------------------------------------------------
%%% @author savannahallsop
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2019 14:40
%%%-------------------------------------------------------------------
-module(riak_stat_admin).
-author("savannahallsop").

%% API
-export([]).

-export([coordinate/2]).

%% find entries is in this module as it calls into the exometer module
%% but uses the sanitised data from riak_stat_data

%% register_stats -> metadata -> exometer


coordinate(Fun, Arg) ->
  Fun1 = riak_stat_data:sanitise_func(Fun),
  case Fun1 of
    no_function_found ->
      no_function_found(Arg);
    register ->
      register(Arg);
    update ->
      update(Arg);
    read ->
      read(Arg)
  end.

no_function_found(_Info) ->
  {error, no_function_found}.

register(Arg) ->
  Arg.

update(Arg) ->
  Arg.

read(Arg) ->
  Arg.
