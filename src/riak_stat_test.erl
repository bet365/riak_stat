%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 05. Jul 2019 09:52
%%%-------------------------------------------------------------------
-module(riak_stat_test).
-include("eunit/include/eunit.hrl").

%% API
%%-export([]).

%% https://learnyousomeerlang.com/eunit#the-need-for-tests

%% metadata
-export([riak_stat_metadata_get_test/0]).

%% TODO START

%% set up fixtures for registering a stat, and then deleting the stat
%% set up fixtures for updating a stat constantly,
%% set up fixtures for updating from multiple nodes at once.

%% TODO END

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% SETUP FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% ACTUAL TESTS %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% HELPER FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%% riak_stat %%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% SETUP FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% ACTUAL TESTS %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% HELPER FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%% riak_stat_console %%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% SETUP FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% ACTUAL TESTS %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% HELPER FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%% riak_stat_profiles %%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% SETUP FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% ACTUAL TESTS %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% HELPER FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%% riak_stat_admin %%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% SETUP FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% ACTUAL TESTS %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% HELPER FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%% riak_stat_data %%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% SETUP FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% ACTUAL TESTS %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% HELPER FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%% riak_stat_info %%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% SETUP FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% ACTUAL TESTS %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% HELPER FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%% riak_stat_cache %%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% SETUP FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% ACTUAL TESTS %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% HELPER FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%% riak_stat_coordinator %%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% SETUP FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% ACTUAL TESTS %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% HELPER FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%% riak_stat_exometer %%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% SETUP FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% ACTUAL TESTS %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% HELPER FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%% riak_stat_metadata %%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% SETUP FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% ACTUAL TESTS %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% HELPER FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%% basic API

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% SETUP FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% ACTUAL TESTS %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% HELPER FUNCTIONS %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

riak_stat_metadata_get_test() ->
  {Type, Opts, Aliases} = riak_stat_metadata:get(
    {riak_core_nodeid:get(), stats}, [riak,riak_core,ignored_gossip_total]),
  Type == counter,
  true == is_list(Opts),
  true == is_list(Aliases), Aliases == [{value, ignored_gossip_total}],
  ok.

riak_stat_metadata_put_test() ->
  ok.

riak_stat_metadata_delete_test() ->
  ok.

%%%%%%% API

riak_stat_metadata_check_meta_test() ->
  ok.

riak_stat_metadata_check_status_test() ->
  ok.

riak_stat_metadata_change_status_test() ->
  % change_status/1 and change_status/2
  ok.

riak_stat_metadata_set_options_test() ->
  % set_options/2 and /4
  ok.

%%%%%% Admin API

riak_stat_metadata_register_stat_test() ->
  % register_stat/1 and /4
  ok.

riak_stat_metadata_unregister_stat_test() ->
  ok.

riak_stat_metadata_reset_stat_test() ->
  ok.

riak_stat_metadata_reset_reset_test() ->
  ok.

%%%%%% Profile API

riak_stat_metadata_save_profile_test() ->
  ok.

riak_stat_metadata_load_profile_test() ->
  ok.

riak_stat_metadata_delete_profile_test() ->
  ok.

riak_stat_metadata_reset_profiles_test() ->
  ok.