%%%-------------------------------------------------------------------
%%% @doc
%%% Functions from _stat modules or from to register, update, read
%%% or unregister a stat.
%%% @end
%%%-------------------------------------------------------------------
-module(riak_stat_admin).

-include("riak_stat.hrl").

-behaviour(gen_server).

%% API
-export([
    get_stats/0,
    get_stat/1,
    get_stat_value/1,
    get_app_stats/1,
    get_stats_values/1,
    get_stats_info/1,
    aggregate/2,
    register/3,
    unregister/5,
    update/3
]).

%% Other API
-export([
    data_sanitise/1,
    print/1,
    print/2
]).

%% API
-export([
    start_link/0
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
-define(PFX, riak_stat:prefix()).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(get_stats() -> stats()).
%% @doc
%% get all the stats from metadata (if enabled) or exometer, like
%% "riak-admin stat show riak.**"
%% @end
get_stats() ->
    {_N, MatchSpec, _DPs} = data_sanitise([?PFX ++ "**"], '_', '_'),
    print(gen_server:call(?SERVER, {get_stats, MatchSpec})).

%%% ------------------------------------------------------------------

-spec(get_stat(stats()) -> statlist()).
%% @doc
%% give A Path to a particular stat such as : [riak,riak_kv,node,gets,time]
%% to retrieve the stat
%% @end
get_stat(Path) ->
    print(find_entries([Path], '_')).

-spec(get_stat_value(data()) -> value()).
%% @doc
%% Get the stat(s) value from exometer, only returns enabled values
%% @end
get_stat_value(Arg) ->
    {Names, _MatchSpec, _DP} = data_sanitise(Arg),
    Entries = find_entries(Names, '_'),
    print([find_stat_value(Entry) || {Entry, _} <- Entries]).

%%% ------------------------------------------------------------------

-spec(get_app_stats(app()) -> stats()).
%% @doc
%% similar to get_stats but specific to the app provided in the module
%% "riak-admin stat show riak.<app>.**"
%% @end
get_app_stats(App) ->
    {_N, MatchSpec, _DPs} = data_sanitise([?PFX, App, "**"], '_', '_'),
    print(gen_server:call(?SERVER, {get_stats, MatchSpec})).

-spec(get_stats_values(app()) -> statlist()).
%% @doc
%% Get the stats for the app and all their values
%% @end
get_stats_values(App) ->
    AppStats = get_app_stats(App),
    print([find_stat_value(Stat) || {Stat, _Status} <- AppStats]).

-spec(get_stats_info(app()) -> stats()).
%% @doc
%% get the info for the apps stats from exometer
%% @end
get_stats_info(App) ->
    AppStats = get_app_stats(App),
    print([find_stat_info(Stat) || {Stat, _Status} <- AppStats]).

%%% ------------------------------------------------------------------

-spec(aggregate(pattern(), datapoint()) -> statlist()).
%% @doc
%% @see exometer:aggregate
%% Does the average of stats averages
%% @end
aggregate(Stats, DPs) ->
    Pattern = {{Stats, '_', '_'}, [], [Stats]},
    AggStats = riak_stat_coordinator:aggregate(Pattern, DPs),
    print(AggStats).

%%% ------------------------------------------------------------------

-spec(register(pfx(), app(), statname()) -> ok | error()).
%% @doc
%% register apps stats into both meta and exometer
%% @end
register(P, App, Stat) ->
    gen_server:call(?SERVER, {register, P, App, Stat}).

%%-spec(update(statname(), incrvalue(), type()) -> ok | error()).
%%%% @doc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Moved so update calls straight to coordinator to avoid this gen_server %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% update a stat in exometer, if the stat doesn't exist it will
%%%% re_register it. When the stat is deleted in exometer the status is
%%%% changed to unregistered in metadata, it will check the metadata
%%%% first, if unregistered then ok is returned by default and no stat
%%%% is created.
%%%% @end
%%update(Name, Inc, Type) ->
%%    riak_stat_coordinator:update(Name, Inc, Type).



-spec(unregister(pfx(), app(), Mod :: data(), Idx :: data(), type()) ->
    ok | error()).
%% @doc
%% unregister a stat from the metadata leaves it's status as
%% {status, unregistered}, and deletes the metric from exometer
%% @end
unregister(Pfx, App, Mod, Idx, Type) ->
    gen_server:call(?SERVER, {unregister, {Pfx, App, Mod, Idx, Type}}).


%%%===================================================================
%%% Other API
%%%==================================================================

%% @doc
%% sanitise the data to a form useful for finding the stats in metadata
%% and in exometer
%% @end
data_sanitise(Arg) ->
    riak_stat_data:data_sanitise(Arg).
data_sanitise(Data, Type, Status) ->
    riak_stat_data:data_sanitise(Data, Type, Status).


-spec(print(data(), attr()) -> value()).
print(Arg) ->
    print(Arg, []).
print(Entries, Attr) when is_list(Entries) ->
    lists:map(fun(E) -> print(E, Attr) end, Entries);
print(Entries, Attr) ->
    riak_stat_info:print(Entries, Attr).

%%%===================================================================
%%% Coordinator API
%%%===================================================================

find_entries(Stats, Status) ->
    riak_stat_coordinator:find_entries(Stats, Status).

select_entries(MatchSpec) ->
    riak_stat_coordinator:select(MatchSpec).

find_stat_value(Path) ->
    riak_stat_coordinator:find_stats_info(Path, [value]).

find_stat_info(Stat) ->
    Info = [name, type, module, value, cache, status, timestamp, options],
    riak_stat_coordinator:find_stats_info(Stat, Info).

%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({get_stats, AllStats}, _From, State) ->
    {reply, select_entries(AllStats), State};
handle_call({register, {P, App, Stats}}, _From, State) ->
    lists:foreach(fun(Stat) ->
        register_stat(P, App, Stat)
                  end, Stats),
    {reply, ok, State};
handle_call({unregister, Pfx, App, Mod, Idx, Type}, _From, State) ->
    unreg_stats(Pfx, App, Type, Mod, Idx),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
      State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

register_stat(P, App, Stat) ->
    {Name, Type, Opts, Aliases} =
        case Stat of
            {N, T} -> {N, T, [], []};
            {N, T, Os} -> {N, T, Os, []};
            {N, T, Os, As} -> {N, T, Os, As}
        end,
    StatName = stat_name(P, App, Name),
    NewOpts = add_cache(Opts),
    % Pull out the status of the stat from MetaData
    riak_stat_coordinator:register({StatName, Type, NewOpts, Aliases}).

add_cache(Opts) ->
  case lists:keyfind(cache, 1, Opts) of
    false ->
      lists:keystore(cache, 1, Opts, ?CACHE);
    _ ->
      lists:keyreplace(cache, 1, Opts, ?CACHE)
  end.

% Put the prefix and App name in front of every stat name.
stat_name(P, App, N) when is_atom(N) ->
    stat_name_([P, App, N]);
stat_name(P, App, N) when is_list(N) ->
    stat_name_([P, App | N]).

stat_name_([P, [] | Rest]) -> [P | Rest];
stat_name_(N) -> N.


unreg_stats(P, App, Type, [Op, time], Index) ->
    unreg_stats_([P, App, Type, Op, time, Index]);
unreg_stats(P, App, Type, Mod, Index) ->
    unreg_stats_([P, App, Type, Mod, Index]).

unreg_stats_(StatName) ->
    riak_stat_coordinator:unregister(StatName).
