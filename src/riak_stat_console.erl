%%%-------------------------------------------------------------------
%%% @doc
%%% calls from riak_core_console are directed to this module to
%%% enable/disable or read stats from exometer/metadata
%%%
%%% calls from exoskeleskin point to this module to retrieve stats
%%% for a UDP or HTTP endpoint
%%% @end
%%%-------------------------------------------------------------------
-module(riak_stat_console).

-include("riak_stat.hrl").

-behaviour(gen_server).

%% API
-export([
    show_stat/2,
    show_stat_0/1,
    stat_info/1,
    disable_stat_0/1,
    status_change/2,
    reset_stat/1
]).

%% API
-export([start_link/0
]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(show_stat(data(), status()) -> value()).
%% @doc
%% Show enabled or disabled stats
%% when using riak-admin stat show riak.** enabled stats will show by default
%%
%% otherwise use: riak-admin stat show-enabled | show-disabled
%% @end
show_stat(Arg, Status) ->
    StatsNames = data_sanitise(Arg),
    Stats = gen_server:call(?SERVER, {show, StatsNames, Status}),
    print_stats(Stats).

-spec(show_stat_0(data()) -> value()).
%% @doc
%% Check which stats in exometer are not updating, only checks enabled
%% @end
show_stat_0(Arg) ->
    StatNames = data_sanitise(Arg),
    NotUpdating = gen_server:call(?SERVER, {show_stat_0, StatNames}),
    print_stats(NotUpdating).

-spec(stat_info(data()) -> value()).
%% @doc
%% Returns all the stats information
%% @end
stat_info(Arg) ->
    {Attrs, RestArg} = pick_info_attrs(Arg),
    StatNames = data_sanitise(RestArg),
    Found = gen_server:call(?SERVER, {stat_info, StatNames, Attrs}),
    print_stats(Found).

-spec(disable_stat_0(data()) -> ok).
%% @doc
%% Similar to the function above, but will disable all the stats that
%% are not updating
%% @end
disable_stat_0(Arg) ->
    StatNames = data_sanitise(Arg),
    gen_server:call(?SERVER, {disable_stat_0, StatNames}).

-spec(status_change(data(), status()) -> ok).
%% @doc
%% change the status of the stat (in metadata and) in exometer
%% @end
status_change(Arg, ToStatus) ->
    Stats = data_sanitise(Arg),
    gen_server:call(?SERVER, {change_status, Stats, ToStatus}).

-spec(reset_stat(data()) -> ok).
%% @doc
%% resets the stats in metadata and exometer and tells metadata that the stat
%% has been reset
%% @end
reset_stat(Arg) ->
    StatNames = data_sanitise(Arg),
    gen_server:call(?SERVER, {reset, StatNames}).

%%%===================================================================
%%% Admin API
%%%===================================================================

data_sanitise(Arg) ->
    riak_stat_admin:data_sanitise(Arg).

print_stats(Entries) ->
    print_stats(Entries, []).
print_stats(Entries, Attributes) ->
    riak_stat_admin:print(Entries, Attributes).


%%%===================================================================
%%% Coordinator API
%%%===================================================================

find_entries(StatNames, Status) ->
    riak_stat_coordinator:find_entries(StatNames, Status).

not_updating(StatNames) ->
    riak_stat_coordinator:find_static_stats(StatNames).

find_stat_info(Stats, Info) ->
    riak_stat_coordinator:find_stats_info(Stats, Info).

change_status(Stats) ->
    riak_stat_coordinator:change_status(Stats).

reset_stats(Name) ->
    riak_stat_coordinator:reset_stat(Name).

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
handle_call({show, Arg, Status}, _From, State) ->
    Reply = find_entries(Arg, Status),
    {reply, Reply, State};
handle_call({show_stat_0, Arg}, _From, State) ->
    Reply = not_updating(Arg),
    {reply, Reply, State};
handle_call({stat_info, Arg, Attrs}, _From, State) ->
    Reply = find_stat_info(Arg, Attrs),
    {reply, Reply, State};
handle_call({disable_stat_0, Arg}, _From, State) ->
    NotUpdating = not_updating(Arg),
    DisableTheseStats =
    lists:map(fun({Name, _V}) ->
        {Name, {status, disabled}}
            end, NotUpdating),
    change_status(DisableTheseStats),
    {reply, ok, State};
handle_call({change_status, CleanStats, ToStatus}, _From, State) ->
    Entries = % if disabling lots of stats, pull out only enabled ones
    case ToStatus of
        enabled -> find_entries(CleanStats, disabled);
        disabled -> find_entries(CleanStats, enabled)
    end,
    change_status([{Stat, {status, Status}} || {Stat, Status} <- Entries]),
    {reply, ok, State};
handle_call({reset, Stats}, _From, State) ->
    reset_stats(find_entries(Stats, enabled)),
    {reply, ok, State};
handle_call({Request, _Arg}, _From, State) ->
    {reply, {error, Request}, State};
handle_call(Request, _From, State) ->
    {reply, {error, Request}, State}.

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
%%% Helper functions
%%%===================================================================

-spec(pick_info_attrs(data()) -> value()).
%% @doc get list of attrs to print @end
pick_info_attrs(Arg) ->
    case lists:foldr(
        fun ("-name", {As, Ps}) -> {[name | As], Ps};
            ("-type", {As, Ps}) -> {[type | As], Ps};
            ("-module", {As, Ps}) -> {[module | As], Ps};
            ("-value", {As, Ps}) -> {[value | As], Ps};
            ("-cache", {As, Ps}) -> {[cache | As], Ps};
            ("-status", {As, Ps}) -> {[status | As], Ps};
            ("-timestamp", {As, Ps}) -> {[timestamp | As], Ps};
            ("-options", {As, Ps}) -> {[options | As], Ps};
            (P, {As, Ps}) -> {As, [P | Ps]}
        end, {[], []}, split_arg(Arg)) of
        {[], Rest} ->
            {[name, type, module, value, cache, status, timestamp, options], Rest};
        Other ->
            Other
    end.

split_arg([Str]) ->
    re:split(Str, "\\s", [{return, list}]).