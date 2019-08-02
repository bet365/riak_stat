%%%-------------------------------------------------------------------
%%% @doc
%%% the hub of sanitising Arugments passed into exoskeleskin and sending
%%% data received from console to where it needs to go, retrieving data
%%% for udp and/or http functions as well a function calls to riak_stat
%%% to poll stats.
%%% @end
%%%-------------------------------------------------------------------
-module(exoskele_console).
-include("exoskeleskin.hrl").

-export([
    setup/1,
    setdown/0,
    setdown/1,
    get_host/1,
    get_stats/0
]).

-type hostarg()     :: atom().

%%-------------------------------------------------------------------
-spec(setup(arg()) -> ok).
%% @doc
%% sanitise the argument passed in to retrieve the data from console,
%% passing in information is optional as all the defaults are pulled out from
%% the sys.config.
%% similar to other stats functions the stats can be passed in to only poll
%% those stats, however, only the enabled stats will return with values
%%
%% If the server is already setup and polling stats, setting up another will
%% "restart" the gen_server.
%% @end
setup(Arg) ->
      {{Port, Instance, Sip}, STATSorPROFILES}    =     sanitise_data(Arg),
      start_server(exoskele_udp, {{Port, Instance, Sip}, STATSorPROFILES}).

sanitise_data([<<>>]) -> sanitise_data([]);
sanitise_data(Arg) -> exoskele_data:sanitise_data(Arg).

start_server(Child, Arg) ->
    case get_child() of
        {error, Reason} ->
            lager:error("Couldn't find Children because: ~p~n", [Reason]);
        ChildRef ->
            Ids = [Id || {Id, _Child, _Type, _Mods} <- ChildRef],
            terminate_server(Ids),
            exoskele_sup:start_server(Child, Arg)
    end.


%%-------------------------------------------------------------------
-spec(setdown(arg()) -> ok).
%% @doc
%% Stop the pushing of stats by taking the setup of the udp gen_server
%% down
%% @end
setdown() ->
    setdown([]).
setdown(_Arg) ->
    [terminate_server(ChildId) || {ChildId, _Child, _Type, _Mod} <- get_child()].

get_child() ->
    exoskele_sup:what_kids().

terminate_server(Child) ->
    exoskele_sup:terminate(Child).


%%-------------------------------------------------------------------
-spec(get_host(hostarg()) -> {socket(), server_ip() | server(), port()}).
%% @doc
%% get the host details of the udp_socket or http request details, similar to the state
%% in a gen_server but kept in an ets - table to preserve it longer that the udp
%% gen_server, information is pulled out like a last known request
%% @end
get_host(Info) ->
    case ets:lookup(?EXOSKELETABLE, Info) of
        [{_, {MonitorServer, undefined, MonitorPort, Socket}}] ->
            {Socket, MonitorServer, MonitorPort};
        [{_, {_MonitorServer, MonitorServerIp, MonitorPort, Socket}}] ->
            {Socket, MonitorServerIp, MonitorPort};
        [] ->
            {error, no_info}
    end.

-spec(get_stats() -> stats()).
%% @doc
%% retrieve all the stats out of riak_kv_status
%% todo: improve the stat collection method
%% @end
get_stats() ->
    case riak_stat_config:get_env(exoskele_stats_default, classic) of
        classic ->
            riak_kv_status:get_stats(web);
        beta ->
            riak_stat_coordinator:get_values(['_'])
    end.