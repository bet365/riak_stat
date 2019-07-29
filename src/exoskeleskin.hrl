%%%-------------------------------------------------------------------
%%% @doc
%%% Header File for exoskeleskin - all defaults and config is retrievable
%%% @end
%%%-------------------------------------------------------------------

-define(APP, exoskeleskin).

-define(STATS_UPDATE_INTERVAL, riak_stat_config:get_env(exoskeleskin_stats_update_interval, 1000)).
-define(REFRESH_INTERVAL,      riak_stat_config:get_env(exoskeleskin_ip_refresh_interval, 30000)).

-define(SPIRAL_TIME_SPAN,      riak_stat_config:get_env(exoskeleskin_stats_spiral_time_span, 1000)).
-define(HISTOGRAM_TIME_SPAN,   riak_stat_config:get_env(exoskeleskin_stats_histogram_time_span, 1000)).

%% OPTIONAL ENVIRONMENT VARIABLES

-define(EXCLUDED_DATAPOINTS,   riak_stat_config:get_env(exoskeleskin_excluded_datapoints, [ms_since_reset])).
-define(STATS_LISTEN_PORT,     riak_stat_config:get_env(stats_listen_port, 9000)).
-define(EXOSKELETABLE,         exoskeleskin_state).

-define(INSTANCE,              riak_stat_config:get_env(instance)).
-define(MONITOR_SERVER,        riak_stat_config:get_env(monitor_server)).
-define(MONITOR_LATENCY_PORT,  riak_stat_config:get_env(monitor_latency_port)).
-define(MONITOR_STATS_PORT,    riak_stat_config:get_env(monitor_stats_port)).

-type protocol()          :: udp | http.
-type socket()            :: inet:socket().
-type latency_port()      :: inet:port_number().
-type instance()          :: string().
-type server_ip()         :: inet:ip4_address().
-type server()            :: inet:ip4_address().
-type hostname()          :: inet:hostname().

-type sanitised_data()     :: {protocol(), socket(), latency_port(), port(),
                              instance(), server(), server_ip()}.
-type reason()        :: any().
-type error()         :: {error, reason()}.