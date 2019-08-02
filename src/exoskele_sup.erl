%%%-------------------------------------------------------------------
%%% @doc
%%% Setting up a gen_server to push stats to the endpoint provided
%%% @end
%%%-------------------------------------------------------------------
-module(exoskele_sup).
-behaviour(supervisor).
-include("exoskeleskin.hrl").
-include("riak_stat.hrl").

%% API
-export([
    start_link/0,
    what_kids/0,
    start_server/2,
    stop_server/1
]).
-export([
    init/1,
    terminate/1
]).

-define(SUPERVISOR, ?MODULE).

-define(RESTART, temporary).
-define(SHUTDOWN, 6000).
-define(TYPE, worker).
-define(CHILD(Name),      {Name, {Name, start_link, []},    ?RESTART, ?SHUTDOWN, ?TYPE, [Name]}).
-define(CHILD(Name, Arg), {Name, {Name, start_link, [Arg]}, ?RESTART, ?SHUTDOWN, ?TYPE, [Name]}).

-type child()       :: supervisor:child() | supervisor:child_id().
-type children()    :: [child()].
-type childname()   :: atom().
-type childarg()    :: {{port(), instance(), server_ip()}, stats()}.

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

what_kids() ->
    supervisor:which_children(?SUPERVISOR).

%%%-------------------------------------------------------------------

-spec(terminate(children()) -> ok).
%%% @doc
%%% Terminate the server(s) that are pushing stats to an endpoint
%%% @end
terminate(Children) when is_list(Children) ->
    lists:foreach(fun(Child) ->
        stop_server(Child)
                  end, Children).

stop_server(Mod) ->
    supervisor:terminate_child(?SUPERVISOR, Mod), ok.

%%%-------------------------------------------------------------------
-spec(start_server(childname(), childarg()) -> ok).
%%% @doc
%%% Start up a udp or http gen_server to get stats from exometer and
%%% send to an endpoint given.
%%% @end
start_server(Name, Arg) ->
    Ref = ?CHILD(Name, Arg),
    case supervisor:start_child(?SUPERVISOR, Ref) of
        {ok, Child} -> Child;
        {error, {already_started, Child}} -> Child
    end.


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
%%%--------------------------------------------------------------------
%%% @private
%%% @doc
%%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%%% this function is called by the new process to find out about
%%% restart strategy, maximum restart frequency and child
%%% specifications.
%%%
%%% @end
%%%--------------------------------------------------------------------

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, []}}.
