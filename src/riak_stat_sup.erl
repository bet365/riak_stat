-module(riak_stat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1, terminate/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 6000,
    Type = worker,

    ConsoleChild =
        {console,
            {riak_stat_console, start_link, []},
            Restart, Shutdown, Type, [riak_stat_console]},

    AdminChild =
        {admin,
            {riak_stat_admin, start_link, []},
            Restart, Shutdown, Type, [riak_stat_admin]},

    ProfileChild =
        {profiles,
            {riak_stat_profiles, start_link, []},
            Restart, Shutdown, Type, [riak_stat_profiles]},


    {ok, {SupFlags, [AdminChild, ConsoleChild, ProfileChild]}}.


terminate(Children) ->
    lists:foreach(fun(Child) ->
    supervisor:terminate(riak_stat_sup, Child)
                  end, Children).

