%%%-------------------------------------------------------------------
%%% @doc
%%% console arguments are sent to this module to sanitise data and to
%%% start/terminate gen_servers
%%% @end
%%%-------------------------------------------------------------------
-module(exoskele_console).

-export([
    setup/1,
    setdown/1
]).





setup(Arg) ->
    {Protocol, Port, Instance, Sip} = sanitise_data(Arg),
    Name = protocol_name(Protocol),
    start_server(Name, {Port, Instance, Sip}).

sanitise_data(Arg) ->
    exoskele_data:sanitise_data(Arg).

protocol_name(Protocol) ->
    Proto = re:split([<<"exoskele_">>|[Protocol]], "\\."),
    binary_to_atom(Proto, latin1).

start_server(Child, Arg) ->
    exoskele_sup:start_server(Child, Arg).

setdown([]) ->
    Children = get_child(),
    terminate_server(Children);
setdown(Arg) ->
    {Protocol, _Port, _Instance, _Sip} = sanitise_data(Arg),
    Name = protocol_name(Protocol),
    terminate_server(Name).

get_child() ->
  exoskele_sup:what_kids().

terminate_server(Child) ->
    exoskele_sup:terminate(Child).


get_stats_and_values() ->
  Stats = riak_stat_admin:get_stats(),
  [{Stat, riak_stat_coordinator:find_stats_info(Stat, [value])} ||
    {Stat, _Status} <- Stats].

;