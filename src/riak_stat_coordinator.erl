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

%%
%%
%% all calls to register -> admin -> metadata -> exometer
%%
%% all calls to update
%%
%% all calls to read the stats come through here.