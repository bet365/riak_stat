%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(riak_stat_test).
-include_lib("eunit/include/eunit.hrl").

%% metadata
-compile(export_all).

%% TODO START

%% set up fixtures for registering a stat, and then deleting the stat
%% set up fixtures for updating a stat constantly,
%% set up fixtures for updating from multiple nodes at once.

%% TODO END

-define(setup(Fun),        {setup, fun setup/0, fun cleanup/1, Fun}).
-define(foreach(Fun),      {foreach, fun setup/0, fun cleanup/1, Fun}).
-define(setupconsole(Fun), {setup, fun setup_console/0, fun cleanup/1, Fun}).
-define(setupprofile(Fun), {setup, fun setup_profile/0, fun cleanup/1, Fun}).
-define(fullsetup(Fun),    {setup, fun setup_all/0, fun cleanup/1, Fun}).

-define(spawn(Test),       {spawn, Test}).
-define(timeout(Test),     {timeout, 120, Test}).
-define(inorder(Test),     {inorder, Test}).
-define(inparallel(Test),  {inparallel, Test}).

-define(unload(Module), meck:unload(Module)).
-define(new(Module), meck:new(Module, [passthrough])).
-define(expect(Module, Fun, Val, Funfun), meck:expect(Module, Fun, Val, Funfun)).
-define(expect(Module, Fun, Funfun), meck:expect(Module, Fun, Funfun)).

-define(consoletest(Desc, Test), {?spawn([{Desc, ?setupconsole(fun(_) -> Test end)}])}).
-define(profiletest(Desc, Test), {?spawn([{Desc, ?setupprofile(fun(_) -> Test end)}])}).
-define(setuptest(Desc, Test), {Desc, ?setup(fun(_) -> Test end)}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% SETUP FUNCTIONS %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup() ->
  catch(?unload(riak_stat)),
  catch(?unload(riak_stat_admin)),
  catch(?unload(riak_stat_data)),
  catch(?unload(riak_stat_info)),
  catch(?unload(riak_stat_coordinator)),
  catch(?unload(riak_stat_exometer)),
  catch(?unload(riak_stat_metdata)),
  ?new(riak_stat),
  ?new(riak_stat_admin),
  ?new(riak_stat_data),
  ?new(riak_stat_info),
  ?new(riak_stat_coordinator),
  ?new(riak_stat_exometer),
  ?new(riak_stat_metadata),

  {ok, Pid} = riak_stat_admin:start_link(),
  [Pid].

setup_console() ->
  catch (?unload(riak_stat_console)),
  ?new(riak_stat_console),
  {ok, Pid} = riak_stat_console:start_link(),
  [Pid | setup()].


setup_profile() ->
  catch (?unload(riak_stat_profiles)),
  ?new(riak_stat_profiles),
  {ok, Pid} = riak_stat_profiles:start_link(),
  [Pid | setup()].

setup_all() ->
  catch (?unload(riak_stat_console)),
  ?new(riak_stat_console),
  {ok, CPid} = riak_stat_console:start_link(),

  catch (?unload(riak_stat_profiles)),
  ?new(riak_stat_profiles),
  {ok, PPid} = riak_stat_profiles:start_link(),

  [PPid, CPid | setup()].

cleanup(Pid) ->
  process_flag(trap_exit, true),
  catch(?unload(riak_stat)),
  catch(?unload(riak_stat_admin)),
  catch(?unload(riak_stat_data)),
  catch(?unload(riak_stat_info)),
  catch(?unload(riak_stat_coordinator)),
  catch(?unload(riak_stat_exometer)),
  catch(?unload(riak_stat_metdata)),
  catch(?unload(riak_stat_console)),
  catch(?unload(riak_stat_profiles)),
  process_flag(trap_exit, false),
  Children = supervisor:which_children(Pid),
  lists:foreach(fun({Child, _n, _o, _b}) -> supervisor:terminate_child(Pid, Child) end, Children).



%%%%%%%%%%%%%%%%% riak_stat %%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% TESTS DESCRIPTIONS %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

register_stats_test_() ->
  ?setuptest("stats from stats modules get registered",
    [
      {"register a stat that already exists", fun test_re_register/0},
      {"register a test stat for counter", fun test_count_register/0},
      {"register a test stat for gauge", fun test_gauge_register/0},
      {"register a test stat for histogram", fun test_histogram_register/0}
    ]).

update_stats_test_() ->
  ?setuptest("update stats from stats modules manually",
    [
      {"update a stat", fun test_update_stat/0},
      {"update a non-existent stat", fun test_update_non_stat/0},
      {"updata a stat twice at the same time", fun test_multi_update/0},
      {"update a stat that is unregistered", fun test_update_unregistered/0}
    ]).

unregister_stats_test_() ->
  ?setuptest("unregistering stats manually",
    [
      {"unregister a stat", fun test_unregister_stat/0},
      {"unregister a non-existent stat", fun test_unregister_non_stat/0},
      {"unregister a stat twice at the same time", fun test_unregister_multi_stat/0}
    ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% ACTUAL TESTS %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_re_register() ->
%%  "register a stat that already exists",
  ?_assertEqual(ok, riak_stat:register(riak_core,
    [{ignored_gossip_total, counter, [], [{value, ignored_gossip_total0}]}])).

test_count_register() ->
%%  "register a test stat for counter",
  ?_assert(ok == riak_stat:register(riak_stat,
    [{test_stat_name, counter, [{status, disabled}], [{one, test_stat_count_one},
      {count, test_stat_count_count}]}])).

test_gauge_register() ->
%%  "register a test stat for gauge"
  ?_assert(ok == riak_stat:register(riak_stat,
    [{test_stat_name_2, gauge, [], [{value, test_stat_gauge}]}])).

test_histogram_register() ->
%%  "register a test stat for histogram",
  ?_assert(ok == riak_stat:register(riak_stat,
    [{test_stat_name_3, histogram, [], [{value, test_stat_histo},
      {max, test_stat_histo_max}, {min, test_stat_histo_min}]}])).


test_update_stat() ->
%%  "update a stat"
  ?_assert().

test_update_non_stat() ->
  ok.

test_multi_update() ->
  ok.

test_update_unregistered() ->
  ok.

test_unregister_stat() ->
  ok.

test_unregister_non_stat() ->
  ok.

test_unregister_multi_stat() ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% HELPER FUNCTIONS %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% % % % % % % % % % % % % % % % % % % % % % %
%%%%%%%%%%%%% riak_stat_console %%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% TESTS DESCRIPTIONS %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

show_stats_test_() ->
  ?consoletest("riak-admin stat show <entry>",
    [ {"Show stat that exists", fun test_show_real_stat/0},
      {"Show stat that doesn't exist", fun test_show_fake_stat/0},
      {"Show all stats", fun test_show_all_stats/0},
      {"Show all enabled stats", fun test_show_enabled_stats/0},
      {"Show all disabled stats", fun test_show_disabled_stats/0}
    ]).

info_stats_test_() ->
  ?consoletest("riak-admin stat info <entry>",
    [
      {"Show stat info of existing stat", fun test_info_real_stat/0},
      {"Show stat info of non-existing stat", fun test_info_fake_stat/0},
      {"Show stat info for all stats", fun test_info_all_stats/0},
      {"Show stat info for all enabled stats", fun test_info_enabled_stats/0},
      {"Show stat info for all disabled stats", fun test_info_disabled_stats/0}
    ]).

show_stats_0_test_() ->
  ?consoletest("riak-admin stat show-0 <entry>",
    [
      {"Show all static stats <entry>", fun test_static_stats/0}
    ]).

disable_stats_0_test_() ->
  ?consoletest("riak-admin stat disable-0 <entry>",
    [
      {"Disable all static stats", fun test_disable_0/0},
      {"Disable static stats twice", fun test_disable_0_again/0}
        ]).

enable_stats_test_() ->
  ?consoletest("riak-admin stat enable <entry>",
    [
      {"Enable stats", fun test_stat_enable/0},
      {"Enable stats twice", fun test_stat_enable_again/0},
      {"Enable stats that don't exist", fun test_fake_stat_enable/0}
    ]).

disable_stats_test_() ->
  ?consoletest("riak-admin stat disable <entry>",
    [
      {"Disable stats", fun test_stat_disable/0},
      {"Disable stats twice", fun test_stat_disable_again/0},
      {"Disable stats that don't exist", fun test_fake_stat_disable/0}
    ]).

reset_stats_test_() ->
  ?consoletest("riak-admin stat reset <entry>",
    [
      {"Reset all stats", fun test_reset_stats/0},
      {"Reset all stats twice", fun test_reset_stats_again/0},
      {"Reset stats that don't exist", fun test_fake_stat_reset/0}
    ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% ACTUAL TESTS %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% riak-admin stat show

test_show_real_stat() ->
  ok.

test_show_fake_stat() ->
  ok.

test_show_all_stats() ->
  ok.

test_show_enabled_stats() ->
  ok.

test_show_disabled_stats() ->
  ok.


%% riak-admin stat info

test_info_real_stat() ->
  ok.

test_info_fake_stat() ->
  ok.

test_info_all_stats() ->
  ok.

test_info_enabled_stats() ->
  ok.

test_info_disabled_stats() ->
  ok.


%% riak-admin stat show-0

test_static_stats() ->
  ok.


%% riak-admin stat disable-0

test_disable_0() ->
  ok.

test_disable_0_again() ->
  ok.


%% riak-admin stat enable

test_stat_enable() ->
  ok.

test_stat_enable_again() ->
  ok.

test_fake_stat_enable() ->
  ok.


%% riak-admin stat disable

test_stat_disable() ->
  ok.

test_stat_disable_again() ->
  ok.

test_fake_stat_disable() ->
  ok.


%% riak-admin stat reset

test_reset_stats() ->
  ok.

test_reset_stats_again() ->
  ok.

test_fake_stat_reset() ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% HELPER FUNCTIONS %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% % % % % % % % % % % % % % % % % % % % % % %
%%%%%%%%%%%%% riak_stat_profiles %%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% TESTS DESCRIPTIONS %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

save_profile_test_() ->
  ?profiletest("riak-admin stat save-profile",
    [
      {"Saving a profile", fun test_save_profile/0},
      {"Saving a profile twice", fun test_save_profile_again/0}
    ]).

load_profile_test_() ->
  ?profiletest("riak-admin stat load-profile",
    [
      {"Loading a profile", fun test_load_profile/0},
      {"Loading an already loaded profile", fun test_load_profile_again/0},
      {"Loading a non-existent profile", fun test_load_fake_profile/0}
    ]).

delete_profile_test_() ->
  ?profiletest("riak-admin stat delete-profile",
    [
      {"Delete a profile", fun test_delete_profile/0},
      {"Delete a non-existent profile", fun test_delete_fake_profile/0}
    ]).

reset_profiles_test_() ->
  ?profiletest("riak-admin stat reset-profiles",
    [
      {"Reset a profile that is loaded", fun test_reset_profiles/0},
      {"Reset without a profile loaded", fun test_reset_without/0}
    ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% ACTUAL TESTS %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% riak-admin stat save-profile

test_save_profile() ->
  ok.

test_save_profile_again() ->
  ok.

%% riak-admin stat load-profile

test_load_profile() ->
  ok.

test_load_profile_again() ->
  ok.

test_load_fake_profile() ->
  ok.

%% riak-admin stat delete-profile

test_delete_profile() ->
  ok.

test_delete_fake_profile() ->
  ok.

%% riak-admin stat reset-profiles

test_reset_profiles() ->
  ok.

test_reset_without() ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% HELPER FUNCTIONS %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% % % % % % % % % % % % % % % % % % % % % % %
%%%%%%%%%%%%%% riak_stat_admin %%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% TESTS DESCRIPTIONS %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% API

admin_get_stats_test_() ->
  ?setuptest("riak_stat_admin:get_stats()",
    [
      {"Return a list of Stats from riak_stat_admin ets", fun test_admin_get_stats/0}
    ]).

admin_get_priority_test_() ->
  ?setuptest("riak_stat_admin:priority()",
    [
      {"Return the currently set priority", fun test_admin_get_priority/0}
    ]).

admin_read_stats_test_() ->
  ?setuptest("riak_stat_admin:read_stats(app)",
    [
      {"Read the stats for riak_core", fun test_admin_read_stats/0},
      {"Read the stats for a app that isnt there", fun test_admin_read_fake_stats/0}
    ]).

admin_parse_info_test_() ->
  ?setuptest("riak_stat_admin:parse_information(Data, Status)",
    [
      {"Data from console", fun test_console_parse_info/0},
      {"Data from profile", fun test_profile_parse_info/0},
      {"Extraterrestial Data", fun test_ET_data_parse_info/0}
    ]).

admin_alpha_stat_test_() ->
  ?setuptest("riak_stat_admin:the_alpha_stat",
    [
      {"I must defeat him, to become the alpha", fun admin_alpha_/0},
      {"We're all alphas now", fun admin_all_alpha/0}
    ]).

admin_find_entries_test_() ->
  ?setuptest("riak_stat_admin:find_entries(Data, Status)",
    [
      {"find entries of known argument", fun test_admin_find_entries/0},
      {"find entries of a known unknown argument", fun test_admin_find_no_entries/0}
    ]).

admin_print_test_() ->
  ?setuptest("riak_stat_admin:print(Data, Status)",
    [
      {"print nothing", fun test_admin_print_0/0},
      {"print_something", fun test_admin_print_entries/0}
    ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% ACTUAL TESTS %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_admin_get_stats() ->
  ok.

test_admin_get_priority() ->
  ok.

test_admin_read_stats() ->
  ok.

test_admin_read_fake_stats() ->
  ok.

test_console_parse_info() ->
  ok.

test_profile_parse_info() ->
  ok.

test_ET_data_parse_info() ->
  ok.

admin_alpha_() ->
  ok.

admin_all_alpha() ->
  ok.

test_admin_find_entries() ->
  ok.

test_admin_find_no_entries() ->
  ok.

test_admin_print_0() ->
  ok.

test_admin_print_entries() ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% HELPER FUNCTIONS %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
