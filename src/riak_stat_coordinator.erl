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


%% riak_core_console calls into this module for stats in the console
%% and for profile functions the sat comes in the form (Arg, Type), where
%% Type :: {console,...} | {profiles,...}
%%
%% for profiles it includes the function name as well
%%
%% for example:
coordinate(Arg0, Type) ->
  Arg = sanitise_data(Arg0),
 case Type of
  {profiles, Fun} ->
    riak_stat_profiles:coordinate(Fun, Arg);
  {console, Fun} ->
    riak_stat_console:coordinate(Fun, Arg);
  {admin, Fun} ->
    riak_stat_admin:coordinate(Fun, Arg);
   _ ->
     lager:error("Could not co-ordinate Type ~n")
  end.
%%
%% all the data gets sanitised before it goes into exometer and the
%% metadata, so we can send the Arg to the data and info modules.

sanitise_data(Arg) ->
    riak_stat_data:sanitise_data(Arg).

%%sanitise_func(Fun) ->
%%    riak_stat_data:sanitise_func(Fun).
%%
%%
%% all calls to register -> admin -> metadata -> exometer
%%
%% all calls to update
%%
%% all calls to read the stats come through here.