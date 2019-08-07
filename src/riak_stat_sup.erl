%%--------------------------------------------------------------------
%% @doc
%% riak_stat_admin is started first, it has riak_stat_console that depends
%% on it for parsing information. riak_stat_profile is started last as it
%% has nothing to depend on it and it depends on no one. it's death affects
%% nothing, however, it is affected by both the death of riak_stat_admin and
%% riak_stat_console.
%% @end
%%--------------------------------------------------------------------
-module(riak_stat_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    terminate/1,
    init/1
]).

-define(SUPERVISOR, ?MODULE).
-define(RESTART, permanent).
-define(SHUTDOWN, 5000).
-define(TYPE, worker).
-define(CHILD(I), {I, {I, start_link, []}, ?RESTART, ?SHUTDOWN, ?TYPE, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

-spec(terminate(Children :: list() | term()) ->
    ok | {error, Reason :: any()}).
%% @doc for emergency purposes @end %% and testing
terminate(Children) when is_list(Children) ->
    lists:foreach(fun(Child) ->
        supervisor:terminate(?SUPERVISOR, Child)
                  end, Children);
terminate(Child) ->
    supervisor:terminate(?SUPERVISOR, Child).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStrategy = rest_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags =
        {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
   
    ProfileChild = ?CHILD(riak_stat_profiles),

    {ok, {SupFlags, [ProfileChild]}}.