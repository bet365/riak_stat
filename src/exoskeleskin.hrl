%%%-------------------------------------------------------------------
%%% @doc
%%% Header File for exoskeleskin - all defaults and config is retrievable
%%% @end
%%%-------------------------------------------------------------------

-define(APP, exoskeleskin).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% MACROS for exoskele_udp

-define(INSTANCE,              riak_stat_config:get_env(instance)).
-define(MONITOR_SERVER,        riak_stat_config:get_env(monitor_server)).
-define(MONITOR_LATENCY_PORT,  riak_stat_config:get_env(monitor_latency_port)).
-define(MONITOR_STATS_PORT,    riak_stat_config:get_env(monitor_stats_port)).

-define(EXCLUDED_DATAPOINTS,   riak_stat_config:get_env(exoskeleskin_excluded_datapoints, [ms_since_reset])).
-define(STATS_LISTEN_PORT,     riak_stat_config:get_env(stats_listen_port, 9000)).
-define(EXOSKELETABLE,         exoskeleskin_state).
-define(UDP_KEY,               udp_socket).

-define(STATS_UPDATE_INTERVAL, riak_stat_config:get_env(exoskeleskin_stats_update_interval, 1000)).
-define(REFRESH_INTERVAL,      riak_stat_config:get_env(exoskeleskin_ip_refresh_interval, 30000)).

-define(SPIRAL_TIME_SPAN,      riak_stat_config:get_env(exoskeleskin_stats_spiral_time_span, 1000)).
-define(HISTOGRAM_TIME_SPAN,   riak_stat_config:get_env(exoskeleskin_stats_histogram_time_span, 1000)).

-define(UDP_OPEN_PORT,         0).
-define(UDP_OPEN_BUFFER,       {buffer, 100*1024*1024}).
-define(UDP_OPEN_SNDBUFF,      {sndbuf, 5*1024*1024}).
-define(UDP_OPEN_ACTIVE,       {active,false}).


%% OPTIONAL ENVIRONMENT VARIABLES





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% types for exoskele_udp

-type jsonprops()         :: [{atom(), any()}].
-type serviceid()         :: string() | binary().
-type correlationid()     :: string() | binary().

-type socket()            :: inet:socket().
-type server()            :: inet:ip4_address().
-type latency_port()      :: inet:port_number().
-type server_ip()         :: inet:ip4_address().
-type stats_port()        :: inet:port_number().
-type hostname()          :: inet:hostname().
-type instance()          :: string().



-type protocol()          :: udp | http.

-type sanitised_data()     :: {protocol(), socket(), latency_port(), port(),
                              instance(), server(), server_ip()}.
-type reason()        :: any().
-type error()         :: {error, reason()}.

