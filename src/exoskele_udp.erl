%%%-------------------------------------------------------------------
%%% @doc
%%% Information taken from exoskeleton,
%%% Send Json object over udp to latency monitoring server
%%%
%%% Gen_server is started to open a socket and keep it alive.
%%% @end
%%%-------------------------------------------------------------------
-module(exoskele_udp).

-include("exoskeleskin.hrl").

-behaviour(gen_server).

-export([
    notify/1,
    notify/2,
    notify/3,
    get_host/0
]).

%% API
-export([
    start_link/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {
    socket        :: inet:socket(),
    server        :: inet:ip4_address(),
    latency_port  :: inet:port_number(),
    server_ip     :: inet:ip4_address(),
    stats_port    :: inet:port_number(),
    hostname      :: inet:hostname(),
    instance      :: string()
}).

-type serviceid()       :: string() | binary().
-type correlationid()   :: string() | binary().
-type jsonprops()       :: [{atom(), any()}].

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Notify the udp latency monitor
%% @end
-spec(notify(jsonprops()) -> ok).
notify(JsonProps) when is_list(JsonProps) ->
    DateTime = exoskele_data:format_time(os:timestamp()),
    try
          case get_host() of
              {Socket, Host, Port} ->
                  Data = build_data_packet(JsonProps, DateTime),
                  gen_udp:send(Socket, Host, Port, Data);
              {error, no_udp_socket} ->
                  lager:error("No UDP socket for sending latency"),
                  ok
          end
    catch Class:Reason  ->
        lager:error("Unable to log latency for json=~p, timestamp=~s, Reason={~p,~p}, Stacktrace=~p~n",
            [JsonProps, DateTime, Class, Reason, erlang:get_stacktrace()])
    end;
notify(JsonProps) ->
    lager:error("Unknown format of JsonProps=~p~n", [JsonProps]).

-spec(notify(serviceid(), correlationid()) -> ok).
notify(ServiceId, CorrelationId)
    when is_list(ServiceId) orelse is_binary(ServiceId)
    andalso is_list(CorrelationId) orelse is_binary(CorrelationId) ->
    notify([
        {service_id, ServiceId},
        {correlation_id, CorrelationId}
    ]);
notify(ServiceId, CorrelationId) ->
    lager:error("Unknown format ServiceId=~p, CorrelationId=~p~n", [ServiceId, CorrelationId]).

-spec(notify(serviceid(), correlationid(), jsonprops()) -> ok).
notify(ServiceId, CorrelationId, JsonProps)
    when is_list(ServiceId) orelse is_binary(ServiceId)
    andalso is_list(CorrelationId) orelse is_binary(CorrelationId)
    andalso is_list(JsonProps) ->
    notify([
        {service_id, ServiceId},
        {correlation_id, CorrelationId}
        | JsonProps
    ]);
notify(ServiceId, CorrelationId, JsonProps) ->
    lager:error("Unknown format ServiceId=~p, CorrelationId=~p, JsonProps=~p~n",
        [ServiceId, CorrelationId, JsonProps]).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Arg :: term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Port, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(Port) ->
    MonitorLatencyPort =
        case Port of
            [] -> ?MONITOR_LATENCY_PORT;
            _ -> Port
        end,
    MonitorServer = ?MONITOR_SERVER,
    MonitorStatsPort   = ?MONITOR_STATS_PORT,
    Hostname           = inet_db:gethostname(),
    Instance           = ?INSTANCE,

    {ok, Socket} = gen_udp:open(0, [
        {buffer, 100*1024*1024},
        {sndbuff, 5*1024*1024},
        {active, false}]),

    ets:insert(exoskeleskin_state,
        {udp_socket, {MonitorServer, undefined, MonitorLatencyPort, Socket}}),

    self() ! refresh_monitor_server_ip,
    erlang:send_after(?STATS_UPDATE_INTERVAL, self(), update_stats),

    {ok, #state{
        socket        = Socket,
        latency_port  = MonitorLatencyPort,
        server        = MonitorServer,
        stats_port    = MonitorStatsPort,
        hostname      = Hostname,
        instance      = Instance}
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_info(update_stats, #state{socket=Socket, stats_port = Port, server_ip = undefined, server = Server, hostname = Hostname, instance = Instance} = State) ->
    dispatch_stats(Socket, Hostname, Instance, Server, Port),
    {noreply, State};
handle_info(update_stats, #state{socket=Socket, stats_port = Port, server_ip = ServerIp, hostname = Hostname, instance = Instance} = State) ->
    dispatch_stats(Socket, Hostname, Instance, ServerIp, Port),
    {noreply, State};
handle_info(refresh_monitor_server_ip, State = #state{socket = Socket, latency_port = LatencyMonitorPort, server = MonitorServer}) ->
    RefreshInterval = ?REFRESH_INTERVAL,
    State1= case inet:gethostbyname(MonitorServer) of
                {ok,{hostent,_Hostname,_,_,_, [MonitorServerIp]}} ->
                    true = ets:insert(exoskeleskin_state, {udp_socket, {MonitorServer, MonitorServerIp, LatencyMonitorPort, Socket}}),
                    State#state{server_ip = MonitorServerIp};
                Other ->
                    lager:warning("Unable to refresh ip address of monitor server due to ~p, retrying in ~p ms~n", [Other, RefreshInterval]),
                    State
            end,
    erlang:send_after(RefreshInterval, self(), refresh_monitor_server_ip),
    {noreply, State1};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

dispatch_stats(Socket, ComponentHostname, Instance, MonitoringHostname, Port) ->
    Metrics = get_stats(),
    case exoskele_js:metrics_to_json(Metrics,
        [{instance, Instance},{hostname, ComponentHostname}], ?EXCLUDED_DATAPOINTS) of
        [] ->
            ok;
        JsonStats ->
            ok = gen_udp:send(Socket, MonitoringHostname, Port, JsonStats)
    end,

    erlang:send_after(?STATS_UPDATE_INTERVAL, self(), update_stats).

get_host() ->
    exoskeleskin:get_host(udp_info).

build_data_packet(Props, DateTime) ->
    [${, exoskele_js:format_fields([{timestamp, DateTime}|Props], []), $}].

get_stats() ->
    riak_stat_coordinator:find_stats_info(['_'], [value]).