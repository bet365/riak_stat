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

%% TODO: pull out of riak_stat the default,
%% or have it passed into this module everytime the function is called.

%% so when a function reaches this point its clear where it is going.

%% API
-export([coordinate/1, coordinate/2]).

%% Metadata API
-export([]).

%% Exometer API
-export([]).

coordinate(Fun) ->
  coordinate(Fun, []).
coordinate(Fun, Arg) ->
  case Fun of
    alias -> alias(Arg);
    aliases -> aliases(Arg);
    select -> select(Arg);
    get_datapoint -> {Name, DP} = Arg, get_datapoint(Name, DP);
    get_info -> {Name, Info} = Arg, get_info(Name, Info);
    get_app_stats -> get_stats(Arg);
    get_stat_info -> get_stat_info(Arg);
    get_stat_status -> get_current_meta_stats();
     % admin %
    register -> NewArg = register_meta_stat(Arg), register_exom_stat(NewArg);
    unregister -> unregister_meta_stat(Arg), unregister_exom_stat(Arg);
    reset_stat -> reset_meta_stat(Arg), reset_exom_stat(Arg);
    change_status -> NewArg = change_meta_status(Arg), change_exom_status(NewArg);
     % profile %
    save_profile -> save_profile(Arg);
    load_profile -> load_profile(Arg);
    delete_profile -> delete_profile(Arg);
    reset_profile -> reset_profile()
  end.

%%%===================================================================
%%% Metadata API
%%%===================================================================

%% Api     %%

get_current_meta_stats() ->
  riak_stat_metadata:get_current_stats().

%% Admin   %%

register_meta_stat(Arg) ->
  riak_stat_metadata:register_stat(Arg).

unregister_meta_stat(Arg) ->
  riak_stat_metadata:unregister(Arg).

reset_meta_stat(Arg) ->
  riak_stat_metadata:reset_stat(Arg).

change_meta_status(Arg) ->
  riak_stat_metadata:change_status(Arg).


%% Profile %%

save_profile(Arg) ->
  riak_stat_metadata:save_profile(Arg).

load_profile(Arg) ->
  riak_stat_metadata:load_profile(Arg).

delete_profile(Arg) ->
  riak_stat_metadata:delete_profile(Arg).

reset_profile() ->
  riak_stat_metadata:reset_profile().

%%%===================================================================
%%% Exometer API
%%%===================================================================

alias(Arg) ->
  riak_stat_exometer:alias(Arg).

aliases({Arg, Value}) ->
  riak_stat_exometer:aliases(Arg, Value).

select(Arg) ->
  riak_stat_exometer:select_stat(Arg).

get_stats(Arg) ->
  riak_stat_exometer:read_stats(Arg).

get_stat_info(Arg) ->
  riak_stat_exometer:get_value(Arg).

get_info(Name, Info) ->
  riak_stat_exometer:info(Name, Info).

get_datapoint(Name, DP) ->
  riak_stat_exometer:get_datapoint(Name, DP).

register_exom_stat({StatName, Type, Opts, Aliases}) ->
  riak_stat_exometer:register_stat(StatName, Type, Opts, Aliases).

unregister_exom_stat(Arg) ->
  riak_stat_exometer:unregister_stat(Arg).

reset_exom_stat(Arg) ->
  riak_stat_exometer:reset_stat(Arg).

change_exom_status(Arg) ->
  riak_stat_exometer:change_status(Arg).

























%%-spec(coordinate(Arg :: term(), Type :: {atom(), atom()}) ->
%%  ok | term() | {error, Reason :: term()}).
%%%% @doc
%%%% Any data from the modules are sent to this function and then sent to the
%%%% necessary module to perform the function needed.
%%%% @end
%%coordinate(WhereTo, Arg) ->
%%  case WhereTo of
%%    metadata ->
%%      riak_stat_metadata:coordinate(Arg);
%%    exometer ->
%%      riak_stat_exometer:coordinate(Arg);
%%    admin ->
%%      riak_stat_admin:coordinate(Arg)
%%  end.


