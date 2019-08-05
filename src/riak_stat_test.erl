%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(riak_stat_test).
-include_lib("eunit/include/eunit.hrl").
-include("exoskeleskin.hrl").
%%-compile([export_all]).

-define(setup(Fun),        {setup,    fun setup/0,          fun cleanup/1, Fun}).
-define(foreach(Fun),      {foreach,  fun setup/0,          fun cleanup/1, Fun}).
-define(setupconsole(Fun), {setup,    fun setup_console/0,  fun cleanup/1, Fun}).
-define(setupprofile(Fun), {setup,    fun setup_profile/0,  fun cleanup/1, Fun}).
-define(fullsetup(Fun),    {setup,    fun setup_all/0,      fun cleanup/1, Fun}).
-define(exoskelesetup(Fun),{setup,    fun setup_exoskele/0, fun cleanup/1, Fun}).

-define(spawn(Test),       {spawn,        Test}).
-define(timeout(Test),     {timeout, 120, Test}).
-define(inorder(Test),     {inorder,      Test}).
-define(inparallel(Test),  {inparallel,   Test}).

-define(unload(Module), meck:unload(Module)).
-define(new(Module),    meck:new(Module, [passthrough])).
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
    [Pid | [setup_stats() | setup_exometer()]].

setup_exometer() ->
    catch(?unload(exometer)),
    catch(?unload(exometer_cpu)),
    catch(?unload(exometer_duration)),
    catch(?unload(exometer_entry)),
    catch(?unload(exometer_folsom)),
    catch(?unload(exometer_function)),
    catch(?unload(exometer_histogram)),
    catch(?unload(exometer_igor)),
    catch(?unload(exometer_info)),
    catch(?unload(exometer_probe)),
    catch(?unload(exometer_proc)),
    catch(?unload(exometer_report_lager)),
    catch(?unload(exometer_report_tty)),
    catch(?unload(exometer_shallowtree)),
    catch(?unload(exometer_slide)),
    catch(?unload(exometer_slot_slide)),
    catch(?unload(exometer_spiral)),
    catch(?unload(exometer_uniform)),
    catch(?unload(exometer_util)),
    ?new(exometer),
    ?new(exometer_cpu),
    ?new(exometer_duration),
    ?new(exometer_entry),
    ?new(exometer_folsom),
    ?new(exometer_function),
    ?new(exometer_histogram),
    ?new(exometer_igor),
    ?new(exometer_info),
    ?new(exometer_probe),
    ?new(exometer_proc),
    ?new(exometer_report_lager),
    ?new(exometer_report_tty),
    ?new(exometer_shallowtree),
    ?new(exometer_slide),
    ?new(exometer_slot_slide),
    ?new(exometer_spiral),
    ?new(exometer_uniform),
    ?new(exometer_util),

    catch(?unload(exometer_admin)),
    catch(?unload(exometer_cache)),
    catch(?unload(exometer_folsom_monitor)),
    catch(?unload(exometer_report)),
    ?new(exometer_admin),
    ?new(exometer_cache),
    ?new(exometer_folsom_monitor),
    ?new(exometer_report),

    {ok, APid}  = exometer_admin:start_link(),
    {ok, CPid}  = exometer_cache:start_link(),
    {ok, FMPid} = exometer_folsom_monitor:start_link(),
    {ok, RPid}  = exometer_report:start_link(),
    [APid,CPid,FMPid,RPid].

setup_metadata() ->
    catch(?unload(riak_core_metadata)),
    catch(?unload(riak_core_metadata_manager)),
    catch(?unload(riak_core_metadata_hashtree)),
    catch(?unload(riak_core_metadata_exchange_fsm)),
    catch(?unload(riak_core_metadata_object)),
    catch(?unload(riak_core_broadcast_handler)),
    catch(?unload(dvvset)),
    ?new(riak_core_metadata),
    ?new(riak_core_metadata_manager),
    ?new(riak_core_metadata_hashtree),
    ?new(riak_core_metadata_exchange_fsm),
    ?new(riak_core_metadata_object),
    ?new(riak_core_broadcast_handler),
    ?new(dvvset),

    catch(?unload(riak_core_broadcast)),
    catch(?unload(riak_core_metadata_manager)),
    catch(?unload(riak_core_metadata_hashtree)),
    ?new(riak_core_broadcast),
    ?new(riak_core_metadata_manager),
    ?new(riak_core_metadata_hashtree),

    {ok, BPid}  = riak_core_broadcast:start_link(),
    {ok, HTPid} = riak_core_metadata_hashtree:start_link(),
    {ok, MPid}  = riak_core_metadata_manager:start_link(),
    [BPid, HTPid, MPid].

setup_console() ->
    catch (?unload(riak_stat_console)),
    ?new(riak_stat_console),
    {ok, Pid} = riak_stat_console:start_link(),
    [setup_metadata() | [Pid | setup()]].


setup_profile() ->
    catch (?unload(riak_stat_profiles)),
    ?new(riak_stat_profiles),
    {ok, Pid} = riak_stat_profiles:start_link(),
    [setup_metadata() | [Pid | setup()]].


setup_exoskele() ->
    catch (?unload(exoskeleskin)),
    catch (?unload(exoskeles_console)),
    catch (?unload(exoskele_data)),
    catch (?unload(exoskele_json)),
    catch (?unload(exoskele_sup)),
    catch (?unload(exoskele_wm)),
    ?new(exoskeleskin),
    ?new(exoskeles_console),
    ?new(exoskele_data),
    ?new(exoskele_json),
    ?new(exoskele_sup),
    ?new(exoskele_wm),
    Arg = {{?MONITOR_LATENCY_PORT, ?INSTANCE, ?MONITOR_SERVER}, ['_']},
    {ok, Pid} = exoskele_udp:start_link(Arg),
    [Pid | setup_all()]. %% exoskele depends on riak_stat

setup_stats() ->
%% use a fake list of stats and register them to know the outcome
%% @see stats/0.
    [riak_stat:register(App, Stats) || {App, Stats} <- register_stats()].



setup_all() ->
    catch (?unload(riak_stat_console)),
    ?new(riak_stat_console),
    {ok, CPid} = riak_stat_console:start_link(),

    catch (?unload(riak_stat_profiles)),
    ?new(riak_stat_profiles),
    {ok, PPid} = riak_stat_profiles:start_link(),

    [setup_metadata() | [PPid | [CPid | setup()]]].

cleanup(Pids) ->
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
    Children = [supervisor:which_children(Pid) || Pid <- Pids],
    lists:foreach(fun({Child, _n, _o, _b}) ->
        [supervisor:terminate_child(Pid, Child) || Pid <- Pids] end, Children).

cleanup_exoskele(Pids) ->
    process_flag(trap_exit, true),
    catch(?unload(exoskele_console)),
    catch(?unload(exoskele_data)),
    catch(?unload(exoskele_udp)),
    catch(?unload(exoskele_wm)),
    catch(?unload(exoskele_json)),
    catch(?unload(exoskeleskin)),
    process_flag(trap_exit, false),
    cleanup(Pids).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% STATS  FUNCTIONS %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

register_stats() ->
  lists:map(fun
              ({[App |Name], Type, Opts, Aliases}) ->
                  {App, {Name, Type, Opts, Aliases}};
              ({[App | Name], Type, Opts}) ->
                  {App, {Name, Type, Opts, []}};
              ({[App | Name], Type}) ->
                  {App, {Name, Type, [], []}}
            end, stats()).

stats() ->
    [
        {[riak_core,ignored_gossip_total],  counter,  [{status, enabled}],
                                                                                [{value, ignored_gossip_total}]},
        {[riak_core,rings_reconciled],      spiral,   [{status, enabled}], [{count, rings_reconciled_total},
                                                                                 {one, rings_reconciled}]},
        {[riak_core,ring_creation_size],    counter,  [{status, enabled}], [{value, ring_creation_size}]},
        {[riak_core,gossip_received],       spiral,   [{status, enabled}], [{one, gossip_received}]},
        {[riak_core,rejected_handoffs],     counter,  [{status, enabled}], [{value, rejected_handoffs}]},
        {[riak_core,handoff_timeouts],      counter,  [{status, disabled}],[{value, handoff_timeouts}]},
        {[riak_core,dropped_vnode_requests],counter,  [{status, disabled}],[{value, dropped_vnode_requests_total}]},
        {[riak_core,converge_delay],        duration, [{status, disabled}],[{mean, converge_delay_mean},
                                                                                 {min, converge_delay_min},
                                                                                 {max, converge_delay_max},
                                                                                 {last, converge_delay_last}]},
        {[riak_core,rebalance_delay],       duration, [{status, disabled}],[{min, rebalance_delay_min},
                                                                                 {max, rebalance_delay_max},
                                                                                 {mean, rebalance_delay_mean},
                                                                                 {last, rebalance_delay_last}]},

        {[common,cpu_stats],                histogram,[{status, disabled},{sample_interval, 5000}],[
                                                                                 {nprocs, cpu_nprocs},
                                                                                 {avg1  , cpu_avg1},
                                                                                 {avg5  , cpu_avg5},
                                                                                 {avg15 , cpu_avg15}]},
        {[common,mem_stats],                spiral,   [{status, enabled}], [{total, mem_total},
                                                                                 {allocated, mem_allocated}]},
        {[common,memory_stats],             spiral,   [{status, enabled}], [{total         , memory_total},
                                                                                 {processes     , memory_processes},
                                                                                 {processes_used, memory_processes_used},
                                                                                 {system        , memory_system},
                                                                                 {atom          , memory_atom},
                                                                                 {atom_used     , memory_atom_used},
                                                                                 {binary        , memory_binary},
                                                                                 {code          , memory_code},
                                                                                 {ets           , memory_ets}]},

        {[riak_repl,last_report],            gauge,  [{status, enabled}],  []}, % repl only has 5 gagues
        {[riak_repl,last_client_bytes_sent], gauge,  [{status, enabled}],  []},
        {[riak_repl,last_client_bytes_recv], gauge,  [{status, enabled}],  []},
        {[riak_repl,last_server_bytes_sent], gauge,  [{status, disabled}], []},
        {[riak_repl,last_server_bytes_recv], gauge,  [{status, disabled}], []},

        {[yz_stat,index,latency],        histogram,  [{status, disabled}], [{min,    search_index_latency_min},
                                                                                 {max,    search_index_latency_max},
                                                                                 {mean,   search_index_latency_mean},
                                                                                 {median, search_index_latency_median},
                                                                                 {95,     search_index_latency_95},
                                                                                 {99,     search_index_latency_99},
                                                                                 {999,    search_index_latency_999}]},
        {[yz_stat,queue,batchsize],    histogram, [{status, disabled}],    [{min,    search_queue_batchsize_min},
                                                                                 {max,    search_queue_batchsize_max},
                                                                                 {mean,   search_queue_batchsize_mean},
                                                                                 {median, search_queue_batchsize_median}]},
        {[yz_stat,queue,drain,latency],histogram, [{status, disabled}], [{min,    search_queue_drain_latency_min},
                                                                              {max,    search_queue_drain_latency_max},
                                                                              {mean,   search_queue_drain_latency_mean},
                                                                              {median, search_queue_drain_latency_median},
                                                                              {95,     search_queue_drain_latency_95},
                                                                              {99,     search_queue_drain_latency_99},
                                                                              {999,    search_queue_drain_latency_999}]},
        {[yz_stat,queue,batch,latency],histogram, [{status, enabled}],  [{min,    search_queue_batch_latency_min},
                                                                              {max,    search_queue_batch_latency_max},
                                                                              {mean,   search_queue_batch_latency_mean},
                                                                              {median, search_queue_batch_latency_median},
                                                                              {95,     search_queue_batch_latency_95},
                                                                              {99,     search_queue_batch_latency_99},
                                                                              {999,    search_queue_batch_latency_999}]},
        {[yz_stat,'query',latency],    histogram, [{status, enabled}],  [{95    , search_query_latency_95},
                                                                              {99    , search_query_latency_99},
                                                                              {999   , search_query_latency_999},
                                                                              {max   , search_query_latency_max},
                                                                              {median, search_query_latency_median},
                                                                              {min   , search_query_latency_min},
                                                                              {mean  , search_query_latency_mean}]},

        {[riak_api,pbc_connects],          spiral, [{status, enabled}], [{one, pbc_connects},
                                                                              {count, pbc_connects_total}]},
        {[riak_api,pbc_connects, active],counter,  [{status, enabled}], [{value, pbc_active}]},




        {[riak_kv,vnode, gets],          spiral,     [{status, enabled}], [{one  , vnode_gets},
                                                                                {count, vnode_gets_total}]},
        {[riak_kv,vnode, gets, time],    histogram,  [{status, disabled}], [{mean  , vnode_get_fsm_time_mean},
                                                                                 {median, vnode_get_fsm_time_median},
                                                                                 {95    , vnode_get_fsm_time_95},
                                                                                 {99    , vnode_get_fsm_time_99},
                                                                                 {max   , vnode_get_fsm_time_100}]},
        {[riak_kv,ignored_gossip_total], counter, [{status, disabled}], [{value, ignored_gossip_total}]},
        {[riak_kv,vnode, heads],         spiral,  [{status, disabled}], [{one, vnode_heads},
                                                                              {count, vnode_heads_total}]},
        {[riak_kv,vnode, heads, time], histogram, [{status, disabled}], [{mean , vnode_head_fsm_time_mean},
                                                                              {median, vnode_head_fsm_time_median},
                                                                              {95    , vnode_head_fsm_time_95},
                                                                              {99    , vnode_head_fsm_time_99},
                                                                              {max   , vnode_head_fsm_time_100}]},
        {[riak_kv,vnode, puts],     spiral,  [{status, disabled}], [{one  , vnode_puts},
                                                                         {count, vnode_puts_total}]},
        {[riak_kv,vnode, puts, time], histogram, [{status, enabled}], [{mean  , vnode_put_fsm_time_mean},
                                                                            {median, vnode_put_fsm_time_median},
                                                                            {95    , vnode_put_fsm_time_95},
                                                                            {99    , vnode_put_fsm_time_99},
                                                                            {max   , vnode_put_fsm_time_100}]},

        {[riak_kv,node, gets],     spiral,  [{status, enabled}], [{one  , node_gets},
                                                                       {count, node_gets_total}]},

        {[riak_kv,node, gets, fsm, active], counter, [{status, enabled}], []},
        {[riak_kv,node, gets, fsm, errors], spiral,  [{status, enabled}], [{one, node_get_fsm_errors},
                                                                                {count, node_get_fsm_errors_total}]},
        {[riak_kv,node, gets, objsize], histogram, [{status, enabled}], [{mean  , node_get_fsm_objsize_mean},
                                                                              {median, node_get_fsm_objsize_median},
                                                                              {95    , node_get_fsm_objsize_95},
                                                                              {99    , node_get_fsm_objsize_99},
                                                                              {max   , node_get_fsm_objsize_100}]},
        {[riak_kv,node, gets, read_repairs],     spiral,  [{status, disabled}], [{one, read_repairs},
                                                                                      {count, read_repairs_total}]},
        {[riak_kv,node, gets, skipped_read_repairs], spiral, [{status, disabled}], [{one, skipped_read_repairs},
                                                                                        {count, skipped_read_repairs_total}]},
        {[riak_kv,node, gets, siblings],     histogram,  [{status, disabled}], [{mean  , node_get_fsm_siblings_mean},
                                                                                      {median, node_get_fsm_siblings_median},
                                                                                      {95    , node_get_fsm_siblings_95},
                                                                                      {99    , node_get_fsm_siblings_99},
                                                                                      {max   , node_get_fsm_siblings_100}]},
        {[riak_kv,node, gets, time], histogram, [{status, disabled}], [{mean  , node_get_fsm_time_mean},
                                                                            {median, node_get_fsm_time_median},
                                                                            {95    , node_get_fsm_time_95},
                                                                            {99    , node_get_fsm_time_99},
                                                                            {max   , node_get_fsm_time_100}]},
        {[riak_kv,node, gets, counter],     spiral,  [{status, disabled}], [{one  , node_gets_counter},
                                                                                  {count, node_gets_counter_total}]},
        {[riak_kv,node, gets, counter, objsize], histogram, [{status, enabled}], [{mean  , node_get_fsm_counter_objsize_mean},
                                                                                        {median, node_get_fsm_counter_objsize_median},
                                                                                        {95    , node_get_fsm_counter_objsize_95},
                                                                                        {99    , node_get_fsm_counter_objsize_99},
                                                                                        {max   , node_get_fsm_counter_objsize_100}]},
        {[riak_kv,node, gets, counter, read_repairs],     spiral,  [{status, enabled}], [{one  , read_repairs_counter},
                                                                                        {count, read_repairs_counter_total}]},
        {[riak_kv,node, gets, counter, siblings], histogram, [{status, enabled}], [{mean  , node_get_fsm_counter_siblings_mean},
                                                                                        {median, node_get_fsm_counter_siblings_median},
                                                                                        {95    , node_get_fsm_counter_siblings_95},
                                                                                        {99    , node_get_fsm_counter_siblings_99},
                                                                                        {max   , node_get_fsm_counter_siblings_100}]},
        {[riak_kv,node, gets, set],     spiral,  [{status, enabled}], [{one  , node_gets_set},
                                                                            {count, node_gets_set_total}]},
        {[riak_kv,counter, actor_count],     histogram,  [{status, disabled}], [{mean  , counter_actor_counts_mean},
                                                                                  {median, counter_actor_counts_median},
                                                                                  {95    , counter_actor_counts_95},
                                                                                  {99    , counter_actor_counts_99},
                                                                                  {max   , counter_actor_counts_100}]},
        {[riak_kv,set, actor_count],     histogram,  [{status, disabled}], [{mean  , set_actor_counts_mean},
                                                                                  {median, set_actor_counts_median},
                                                                                  {95    , set_actor_counts_95},
                                                                                  {99    , set_actor_counts_99},
                                                                                  {max   , set_actor_counts_100}]},
        {[riak_kv,map, actor_count],     histogram,  [{status, disabled}], [{mean  , map_actor_counts_mean},
                                                                                  {median, map_actor_counts_median},
                                                                                  {95    , map_actor_counts_95},
                                                                                  {99    , map_actor_counts_99},
                                                                                  {max   , map_actor_counts_100}]},

        {[riak_pipe,pipeline, create], spiral, [{status, enabled}], [{count, pipeline_create_count},
                                                                          {one, pipeline_create_one}]},
        {[riak_pipe,pipeline, create, error],     spiral,  [{status, disabled}], [{count, pipeline_create_error_count},
                                                                                       {one, pipeline_create_error_one}]},
        {[riak_pipe,pipeline, active],     counter,  [{status, disabled}], [{value, pipeline_active}]}




    ].


% % % % % % % % % % % % % % % % % % % % % % %
%%%%%%%%%%%%%%%%%% console %%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% TESTS DESCRIPTIONS %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% riak-admin stat show <entry>
stat_show_test_() ->
    ?consoletest("riak-admin stat show <entry>",
        [
            {"stat show *.**",                        fun test_stat_show_star/0},
            {"stat show riak.**",                     fun test_stat_show_riak_star/0},
            {"stat show riak.*.node.**",              fun test_stat_show_riak_node_stats/0},
            {"stat show riak.**/status=disabled",     fun test_stat_show_disabled/0},
            {"stat show node_gets",                   fun test_stat_show_legacy_search/0},
            {"stat show riak.riak_kv.node.gets",      fun test_stat_show_node_gets/0},
            {"stat show *.**/type=duration/mean,max", fun test_stat_show_type_dps/0},
            {"stat show not_stat",                    fun test_stat_show_not_stat/0}
        ]).

stat_show_0_test_() ->
    ?consoletest("riak-admin stat show-0 <entry>",
        [
            {"stat show-0 *.**",                      fun test_stat_show0_star/0},
            {"stat show-0 riak.**",                   fun test_stat_show0_riak_star/0},
            {"stat show-0 riak.*.node.**",            fun test_stat_show0_riak_node_stats/0},
            {"stat show-0 node_gets",                 fun test_stat_show0_legacy_search/0},
            {"stat show-0 not_stat",                  fun test_stat_show0_not_stat/0}
        ]).

stat_disable_0_test_() ->
    ?consoletest("riak-admin stat disable-0 <entry>",
        [
            {"stat disable-0 riak.**",                fun test_stat_disable0_riak_star/0},
            {"stat disable-0 riak.*.node.**",         fun test_stat_disable0_riak_node_stats/0},
            {"stat disable-0 node_gets",              fun test_stat_disable0_legacy_search/0},
            {"stat disable-0 riak.riak_kv.node.gets", fun test_stat_disable0_node_gets/0},
            {"stat disable-0 not_stat",               fun test_stat_disable0_not_stat/0}
        ]).

stat_info_test_() ->
    ?consoletest("riak-admin stat info <entry>",
        [
            {"stat show -module *.**",                fun test_stat_info_star_module/0},
            {"stat show -options riak.**",            fun test_stat_info_options_riak_star/0},
            {"stat show riak.*.node.**",              fun test_stat_info_riak_node_stats/0},
            {"stat show -status riak.**",             fun test_stat_info_status/0},
            {"stat show -datapoints node_gets",       fun test_stat_info_legacy_search_dps/0},
            {"stat show riak.riak_kv.node.gets",      fun test_stat_info_node_gets/0},
            {"stat show -type -value *.**",           fun test_stat_info_type_value/0},
            {"stat show not_stat",                    fun test_stat_info_not_stat/0}
        ]).

stat_enable_test_() ->
    ?consoletest("riak-admin stat enable <entry>",
        [
            {"stat enable riak.**",                   fun test_stat_enable_riak_star/0},
            {"stat enable riak.*.node.**",            fun test_stat_enable_riak_node_stats/0},
            {"stat enable node_gets",                 fun test_stat_enable_legacy_search/0},
            {"stat enable riak.riak_kv.node.gets",    fun test_stat_enable_node_gets/0},
            {"stat enable not_stat",                  fun test_stat_enable_not_stat/0}
        ]).

stat_disable_test_() ->
    ?consoletest("riak-admin stat disable <entry>",
        [
            {"stat disable riak.**",                  fun test_stat_disable_riak_star/0},
            {"stat disable riak.*.node.**",           fun test_stat_disable_riak_node_stats/0},
            {"stat disable node_gets",                fun test_stat_disable_legacy_search/0},
            {"stat disable riak.riak_kv.node.gets",   fun test_stat_disable_node_gets/0},
            {"stat disable not_stat",                 fun test_stat_disable_not_stat/0}
        ]).

reset_stats_test_() ->
    ?consoletest("riak-admin stat reset <entry>",
        [
            {"stat reset riak.**",                    fun test_stat_reset_riak_star/0},
            {"stat reset riak.*.node.**",             fun test_stat_reset_riak_node_stats/0},
            {"stat reset node_gets",                  fun test_stat_reset_legacy_search/0},
            {"stat reset riak.riak_kv.node.gets",     fun test_stat_reset_node_gets/0},
            {"stat reset not_stat",                   fun test_stat_reset_not_stat/0}
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% ACTUAL TESTS %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% riak-admin stat show

test_stat_show_star() ->
    ok.

test_stat_show_riak_star() ->
    ok.

test_stat_show_riak_node_stats() ->
    ok.

test_stat_show_disabled() ->
    ok.

test_stat_show_legacy_search() ->
    ok.

test_stat_show_node_gets() ->
    ok.

test_stat_show_type_dps() ->
    ok.

test_stat_show_not_stat() ->
    ok.


%% riak-admin stat show-0

test_stat_show0_star() ->
    ok.

test_stat_show0_riak_star() ->
    ok.

test_stat_show0_riak_node_stats() ->
    ok.

test_stat_show0_legacy_search() ->
    ok.

test_stat_show0_not_stat() ->
    ok.


%% riak-admin stat disable-0

test_stat_disable0_riak_star() ->
      ok.

test_stat_disable0_riak_node_stats() ->
    ok.

test_stat_disable0_legacy_search() ->
    ok.

test_stat_disable0_node_gets() ->
    ok.

test_stat_disable0_not_stat() ->
    ok.


%% riak-admin stat info

test_stat_info_star_module() ->
    ok.

test_stat_info_options_riak_star() ->
    ok.

test_stat_info_riak_node_stats() ->
    ok.

test_stat_info_status() ->
    ok.

test_stat_info_legacy_search_dps() ->
    ok.

test_stat_info_node_gets() ->
    ok.

test_stat_info_type_value() ->
    ok.

test_stat_info_not_stat() ->
    ok.


%% riak-admin stat enable

test_stat_enable_riak_star() ->
    ok.

test_stat_enable_riak_node_stats() ->
    ok.

test_stat_enable_legacy_search() ->
    ok.

test_stat_enable_node_gets() ->
    ok.

test_stat_enable_not_stat() ->
    ok.


%% riak-admin stat disable

test_stat_disable_riak_star() ->
    ok.

test_stat_disable_riak_node_stats() ->
    ok.

test_stat_disable_legacy_search() ->
    ok.

test_stat_disable_node_gets() ->
    ok.

test_stat_disable_not_stat() ->
    ok.


%% riak-admin stat reset

test_stat_reset_riak_star() ->
    ok.

test_stat_reset_riak_node_stats() ->
    ok.

test_stat_reset_legacy_search() ->
    ok.

test_stat_reset_node_gets() ->
    ok.

test_stat_reset_not_stat() ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% HELPER FUNCTIONS %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% % % % % % % % % % % % % % % % % % % % % % %
%%%%%%%%%%%%%%%%%% profiles %%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% TESTS DESCRIPTIONS %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

save_profile_test_() ->
    ?profiletest("riak-admin stat save-profile <entry>",
        [
            {"Saving a profile",                  fun test_save_profile/0},
            {"Saving a profile twice",            fun test_save_profile_again/0}
        ]).

load_profile_test_() ->
    ?profiletest("riak-admin stat load-profile <entry>",
        [
            {"Loading a profile",                 fun test_load_profile/0},
            {"Loading an already loaded profile", fun test_load_profile_again/0},
            {"Loading a non-existent profile",    fun test_load_fake_profile/0}
        ]).

remove_profile_test_() ->
    ?profiletest("riak-admin stat remove-profile <entry>",
        [
            {"Delete a profile",                  fun test_delete_profile/0},
            {"Delete a non-existent profile",     fun test_delete_fake_profile/0}
        ]).

reset_profiles_test_() ->
    ?profiletest("riak-admin stat reset-profiles <entry>",
        [
            {"Reset a profile that is loaded",    fun test_reset_profiles/0},
            {"Reset without a profile loaded",    fun test_reset_without/0}
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
%%%%%%%%%%%%%%%%%%% admin %%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% TESTS DESCRIPTIONS %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% API

read_admin_api_test_() ->
    ?setuptest("riak_stat admin api read functions",
        [
            {"riak_stat:get_stats()",                   fun test_get_stats/0},
            {"riak_stat:get_stat(Path)",                fun test_get_stat/0},
            {"riak_stat:get_value(Arg}",                fun test_get_value/0},
            {"riak_stat:get_app_stats(App)",            fun test_get_app_stats/0},
            {"riak_stat:get_stats_values(App)",         fun test_get_stats_values/0},
            {"riak_stat:get_stats_info(App)",           fun test_get_stats_info/0},
            {"riak_stat:aggregate(Stats, Dps)",         fun test_aggregate_stats/0}
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_admin_api_test_() ->
    ?setuptest("riak_stat admin api create functions",
        [
            {"register a stat that already exists",     fun test_re_register/0},
            {"register an unregistered stat",           fun test_stat_unregister/0}
        ]).

update_admin_api_test_() ->
    ?setuptest("riak_stat admin api update functions",
        [
            {"update a non-existent stat",              fun test_update_non_stat/0},
            {"updata a stat twice at the same time",    fun test_multi_update/0},
            {"update a stat that is unregistered",      fun test_update_unregistered/0}
        ]).

delete_admin_api_test_() ->
    ?setuptest("riak_stat admin api delete functions",
        [
            {"unregister a stat",                       fun test_unregister_stat/0},
            {"unregister a non-existent stat",          fun test_unregister_non_stat/0},
            {"unregister, then enabled metadata again", fun test_unregister_no_meta/0},
            {"unregister a stat twice at the same time",fun test_unregister_multi_stat/0}
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% ACTUAL TESTS %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Read Functions

test_get_stats() ->
    ok.

test_get_stat() ->
    ok.

test_get_value() ->
    ok.

test_get_app_stats() ->
    ok.

test_get_stats_values() ->
    ok.

test_get_stats_info() ->
    ok.

test_aggregate_stats() ->
    ok.

%% Create Functions

test_re_register() ->
    ok.

test_stat_unregister() ->
    ok.

%% Update Functions

test_update_non_stat() ->
    ok.

test_multi_update() ->
    ok.

test_update_unregistered() ->
    ok.

%% Delete Functions

test_unregister_stat() ->
    ok.

test_unregister_non_stat() ->
    ok.

test_unregister_no_meta() ->
    ok.

test_unregister_multi_stat() ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% ACTUAL TESTS %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% HELPER FUNCTIONS %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
