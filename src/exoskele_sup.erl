%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(exoskele_sup).

-behaviour(supervisor).

-include("exoskeleskin.hrl").

%% API
-export([start_link/0,
  start_server/2,
  stop_server/1]).
-export([init/1, terminate/1]).


-define(RESTART, permanent).
-define(SHUTDOWN, 6000).
-define(TYPE, worker).
-define(CHILD(Name), {Name, {Name, start_link, []}, ?RESTART, ?SHUTDOWN, ?TYPE}).
-define(CHILD(Name, Arg), {Name, {Name, start_link, [Arg]}, ?RESTART, ?SHUTDOWN, ?TYPE}).

start_link() ->
  supervisor:start_link({global, ?MODULE}, ?MODULE, []).

start_server(Name, Arg) ->
  Ref = ?CHILD(Name, Arg),
  case supervisor:start_child(?MODULE, Ref) of
    {ok, Child} -> Child;
    {error, {already_started, Child}} -> Child
  end.

stop_server(Mod) ->
  supervisor:terminate_child(?MODULE, Mod), ok.

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  HTTPChild =
  case ?HTTP_ENABLED of
    true ->
      ?CHILD(exoskele_http);
    false ->
      []
  end,

  UDPChild =
  case ?UDP_ENABLED of
    true ->
      ?CHILD(exoskele_udp);
    false ->
      []
  end,

  Children = lists:flatten([HTTPChild | UDPChild]),

  {ok, {SupFlags, [Children]}}.


terminate(Children) ->
  lists:foreach(fun(Child) ->
    supervisor:terminate(riak_stat_sup, Child)
                end, Children).


