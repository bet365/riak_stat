%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(exoskeleskin).

-include("exoskeleskin.hrl").

%% Setup and Setdown
-export([
    get_host/1,
    setup/1,
    disable_port/1
]).

%% API
-export([
    enable/2,
    disable/2,
    change_port/1,
    restart/1,
    restart/2
]).

-spec(get_host(atom()) -> term()).
%% @doc
%% ETS table acts as a permanent state as the gen_servers are temporary,
%% data is pulled out of state as a last known request
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

-spec(setup(term()) -> ok).
%% @doc
%% Set up a gen_server for udp or a gen_server to handle requests from
%% webmachine
%% @end
setup(Arg) ->
    exoskele_console:setup(Arg).

-spec(disable_port(term()) -> term()).
%% @doc
%% remove the servers currently running and pushing stats to an endpoint,
%% removes communication between wm and client
%% @end
disable_port(Arg) ->
    exoskele_console:setdown(Arg).







enable(udp, Por) ->
    Port = exoskelestats:parse_data(Por),
    case get_host() of
        {error, no_udp_socket} ->
            enable_udp(Port);
        {_Socket, _Host, UPort} when UPort == Port ->
            lager:error("UDP already enabled"),
            ok;
        {_Socket, _Host, _UPort} ->
            change_port(udp, Port)
    end;
enable(http, Por) ->
    Port = exoskelestats:parse_data(Por),
    case get_http() of
        {error, no_http_socket} ->
            enable_http(Port);
        {_Sock, _Host, HPort} when HPort == Port ->
            lager:error("Port already open with http~n"),
            ok;
        {_Sock, _Host, _HPort} ->
            change_port(http, Port)
    end.

enable_udp(Port) ->
    exoskele_sup:start_server(exoskele_udp, Port).
enable_http(Port) ->
    exoskele_sup:start_server(exoskele_http, Port).

disable(udp, _Port) ->
    case get_host() of
        {error, no_udp_socket} ->
            ok;
        {_Socket, _Host, _MPort} ->
            disable_udp()
    end,
    ok;
disable(http, _Port) ->
    case get_http() of
        {error, no_http_socket} ->
            ok;
        {_So, _In, _fo} ->
            disable_http()
    end.

disable_udp() ->
    exoskele_sup:stop_server(exoskele_udp).
disable_http() ->
    exoskele_sup:stop_server(exoskele_http).

change_port(Arg) ->
    {Type, Por} = exoskelestats:parse_arg(Arg),
    Port = exoskelestats:parse_data(Por),
    change_port(Type, Port).

change_port(Type, Por) ->
    Port = exoskelestats:parse_data(Por),
      case get_host() of
          {error, no_udp_socket} ->
              case get_http() of
                  {error, no_http_socket} ->
                      case Type of
                          udp ->
                              enable_udp(Port);
                          http ->
                              enable_http(Port);
                          undefined ->
                              io:fwrite("error, type not defined~n")
                      end;
                  _ ->
                      case Type of
                          udp ->
                              disable_http(),
                              enable_udp(Port);
                          http ->
                              restart(http, Port);
                          undefined ->
                              restart(http, Port)
                      end
              end;
            _ ->
              case Type of
                  udp ->
                      enable_udp(Port);
                  http ->
                      disable_udp(),
                      enable_http(Port);
                  undefined ->
                      restart(udp, Port)
              end
    end.

restart(udp, Por) ->
    Port = exoskelestats:parse_data(Por),
    disable_udp(),
    enable_udp(Port),
    lager:info("Port for UDP=~p~n", [Port]);
restart(http, Por) ->
    Port = exoskelestats:parse_data(Por),
    disable_http(),
    enable_http(Port),
    lager:info("Port for HTTP=~p~n", [Port]).

restart(udp) ->
    restart(udp, 0);
restart(http) ->
    restart(http, 0);
restart(Arg)->
    {Type, Port} = exoskelestats:parse_arg(Arg),
    restart(Type, Port).


