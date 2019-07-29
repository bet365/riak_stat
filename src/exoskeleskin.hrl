%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-define(APP, exoskeleskin).

-define(STATS_UPDATE_INTERVAL, riak_stat_config:get_env(exoskeleskin_stats_update_interval, 1000)).
-define(REFRESH_INTERVAL,      riak_stat_config:get_env(exoskeleskin_ip_refresh_interval, 30000)).

-define(SPIRAL_TIME_SPAN,      riak_stat_config:get_env(exoskeleskin_stats_spiral_time_span, 1000)).
-define(HISTOGRAM_TIME_SPAN,   riak_stat_config:get_env(exoskeleskin_stats_histogram_time_span, 1000)).

%% OPTIONAL ENVIRONMENT VARIABLES

-define(SEND_TO_UDP,           riak_stat_config:get_env(send_to_udp, false)).
-define(EXCLUDED_DATAPOINTS,   riak_stat_config:get_env(exoskeleskin_excluded_datapoints, [ms_since_reset])).
-define(SEND_TO_HTTP,          riak_stat_config:get_env(send_to_http, true)).

%%-define(UDP_ENABLED,           riak_stat_config:get_env(udp_enabled, false)).
%%-define(HTTP_ENABLED,          riak_stat_config:get_env(http_enabled, true)).

%% REQUIRED ENVIRONMENT VARIABLES

-define(INSTANCE,              riak_stat_config:get_env(instance)).
-define(MONITOR_SERVER,        riak_stat_config:get_env(monitor_server)).
-define(MONITOR_LATENCY_PORT,  riak_stat_config:get_env(monitor_latency_port)).
-define(MONITOR_STATS_PORT,    riak_stat_config:get_env(monitor_stats_port)).

-type protocol()          :: udp | http.
-type socket()            :: inet:socket().
-type latency_port()      :: inet:port_number().
%%-type port()              :: inet:port_number() | non_neg_integer().
-type instance()          :: string().
-type server_ip()         :: inet:ip4_address().
-type server()            :: inet:ip4_address().
-type hostname()          :: inet:hostname().

-type sanitised_data()     :: {protocol(), socket(), latency_port(), port(),
                              instance(), server(), server_ip()}.
