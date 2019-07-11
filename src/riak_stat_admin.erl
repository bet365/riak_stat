%%%-------------------------------------------------------------------
%%% @doc
%%% Functions from _stat modules or from to register, update, read
%%% or unregister a stat.
%%% @end
%%% Created : 27. Jun 2019 15:08
%%%-------------------------------------------------------------------
-module(riak_stat_admin).
-author("Savannah Allsop").

-include("riak_stat.hrl").

-behaviour(gen_server).

%% API
-export([
  get_stats/0,
  get_stats_status/1,
  get_stats_info/1,
  priority/0,
  read_stats/1,
  get_stat_value/1,
  aggregate/2,
  set_priority/1]).

%% Data API
-export([
  parse_information/2,
  find_entries/2,
  the_alpha_stat/2]).

%% Info API
-export([print/2]).

%% coordinator API
-export([
  register/3,
  unregister/5,
  update/3]).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(PFX, riak_stat:prefix()).

-record(state, {
  statstable,
  priority = metadata}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(get_stats() -> stats()).
%% @doc get all the stats from riak_admin ets table @end
get_stats() ->
  gen_server:call(?SERVER, get_stats).

-spec(priority() -> priority()).
%% @doc return the priority thats save in the riak_admin state @end
priority() ->
  gen_server:call(?SERVER, priority).

%TODO -spec / :: () -> .
get_stat_value(Arg) ->
  riak_stat_coordinator:get_stat_info(Arg).

% TODO: -spec / :: () -> .
aggregate(A, S) ->
  riak_stat_coordinator:aggregate(A, S).

-spec(read_stats(app()) -> stats()).
%% @doc similar to get_stats but specific to the app @end
read_stats(App) ->
  parse_information([?PFX, App], '_').

-spec(get_stats_status(app()) -> stats()).
%% @doc find the status of the apps stats from priority() @end
get_stats_status(App) ->
  Stats = parse_information([?PFX, App], '_'),
  lists:foldl(fun(Stat, Acc) ->
            case riak_stat_coordinator:check_status(Stat) of
              {error, _R} ->
                Acc;
              Stat ->
                [Stat | Acc]
            end
              end, [], Stats).

-spec(get_stats_info(app()) -> stats()).
%% @doc get the info for the apps stats from exometer @end
get_stats_info(App) ->
  Stats = parse_information([?PFX, App], '_'),
  {Attr, _RestArg} = riak_stat_info:pick_info_attrs(Stats),
  lists:foldl(fun(Stat, Acc) ->
    [{Stat, lists:map(fun(At) ->
      {At, riak_stat_coordinator:get_info(Stat, At)}
                      end, Attr)} | Acc]
              end, [], Stats).

-spec(set_priority(value()) -> ok).
%% @doc set priority exom | exometer | meta | metadata, anything other
%% than these options defaults the priority to metadata @end
set_priority(Priority) ->
  gen_server:call(?SERVER, {priority, Priority}).

%%%===================================================================
%%% Data API
%%%===================================================================

-spec(parse_information(Data :: term(), Status :: atom()) -> term()).
%% @doc
%% Data from riak_core_console and other modules arrive in different
%% formats than the format needed, they are sent to this function to
%% be translated into the correct universal format
%%
%% i.e. [<<"riak.riak_kv.node.gets.time">>] ->
%%  [riak,riak_kv,node,gets,time]
%%
%% If there is an input like riak.riak_kv.node.** the ".**" will
%% find any and all stats that follow the first [riak,riak_kv_node]
%% path with anything after it as well.
parse_information(Data, Status) ->
    AList = riak_stat_data:parse_info(Data, Status),
  [parse_pattern(APat0) || {{APat0, _EPat}, _Stuff} <- AList].

parse_pattern(Pat) ->
  APat = [{A, '_'} || A <- Pat],
  gen_server:call(?SERVER, {pattern, APat}).

-spec(find_entries(Arg :: term(), Status :: term()) -> term()).
%% @doc
%% uses the above function to find the data in exometer
%% @end
find_entries(Arg, Status) ->
  Stats = riak_stat_data:find_entries(Arg, Status),
  [print(Stat, print(Attr)) || {Stat, Attr} <- Stats].


-spec(the_alpha_stat(Alpha :: list(), Beta :: list()) -> term()).
%% @doc
%% In the case where one list should take precedent, which is most
%% likely the case when registering in both exometer and metadata, the options
%% hardcoded into the stats may change, or the primary kv for stats statuses
%% switches, in every case, there must be an alpha.
%% @end
the_alpha_stat(Alpha, Beta) ->
% The keys are sorted first with ukeysort which deletes duplicates, then merged
% so any key with the same stat name that is both enabled and disabled returns
% only the enabled option.
  AlphaList = the_alpha_map(Alpha),
  BetaList = the_alpha_map(Beta),
  lists:ukeymerge(2, lists:ukeysort(1, AlphaList), lists:ukeysort(1, BetaList)).
% The stats must fight, to become the alpha

the_alpha_map(A_B) ->
  lists:map(fun
              ({Stat, {Atom, Val}}) -> {Stat, {Atom, Val}};
              ({Stat, Val})         -> {Stat, {atom, Val}};
              ([]) -> []
            end, A_B).


%%%===================================================================
%%% Info API
%%%===================================================================

-spec(print(data(), attr()) -> value()).
print(Arg) ->
  print(Arg, []).
print(Entries, Attr) ->
  riak_stat_info:print(Entries, Attr).

%%%===================================================================
%%% Coordinator API
%%%===================================================================

-spec(register(pfx(), app(), statname()) -> ok | error()).
%% @doc register apps stats into both meta and exometer @end
register(P, App, Stat) ->
  gen_server:call(?SERVER, {register, P, App, Stat}).

-spec(unregister(pfx(), app(), Mod :: data(), Idx :: data(), type()) ->
  ok | error()).
%% @doc unregister a stat from the metadata leaves it's status as
%% {status, unregistered}, and deletes the metric from exometer @end
unregister(Pfx, App, Mod, Idx, Type) ->
  gen_server:call(?SERVER, {unregister, {Pfx, App, Mod, Idx, Type}}).

-spec(update(statname(), incrvalue(), type()) -> ok | error()).
%% @doc update a stat in exometer, if the stat doesn't exist it will
%% re_register it. When the stat is deleted in exometer the status is
%% changed to unregistered in metadata, it will check the metadata
%% first, if unregistered then ok is returned by default and no stat
%% is created. @end
update(Name, Inc, Type) ->
  riak_stat_coordinator:update(Name, Inc, Type).

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
  StatsTable = ets:new(admin, [
    set,
    protected,
    {keypos, 1},
    {write_concurrency, true},
    {read_concurrency, true}
  ]),
  %% No heir, so on startup this ets:table is created
  {ok, #state{statstable = StatsTable}}.

%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call({register, {P, App, Stats}}, _From, State = #state{statstable = ETS}) ->
  lists:foreach(fun(Stat) ->
    register_stat(P, App, Stat, ETS)
                end, Stats),
  {reply, ok, State};
handle_call({unregister, Pfx, App, Mod, Idx, Type}, _From, State) ->
  unreg_stats(Pfx, App, Type, Mod, Idx),
  {reply, ok, State};
handle_call(get_stats, _From, State = #state{statstable = Ets}) ->
  TheStatsList = ets:tab2list(Ets),
  {reply, TheStatsList, State};
handle_call({pattern, Pattern}, _From, State = #state{statstable = Ets}) ->
  Stats =
    lists:foldl(fun(Pat, Acc) ->
      [ets:match_object(Ets, Pat) | Acc]
                end, [], Pattern),
  {reply, Stats, State};
handle_call(priority, _From, State = #state{priority = Priority}) ->
  {reply, Priority, State};
handle_call({priority, Priority}, _From, State = #state{priority = _Whatev}) ->
  NewP =
    case Priority of
      metadata -> metadata;
      meta -> metadata;
      exometer -> exometer;
      exom -> exometer;
      _ -> metadata
    end,
  {reply, ok, State#state{priority = NewP}};
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

register_stat(P, App, Stat, Tab) ->
  {Name, Type, Opts, Aliases} =
    case Stat of
      {N, T} -> {N, T, [], []};
      {N, T, Os} -> {N, T, Os, []};
      {N, T, Os, As} -> {N, T, Os, As}
    end,
  StatName = stat_name(P, App, Name),
  ets:insert(Tab, StatName),
  % Pull out the status of the stat from MetaData
  riak_stat_coordinator:register({StatName, Type, Opts, Aliases}).

% Put the prefix and App name in front of every stat name.
stat_name(P, App, N) when is_atom(N) ->
  stat_name_([P, App, N]);
stat_name(P, App, N) when is_list(N) ->
  stat_name_([P, App | N]).

stat_name_([P, [] | Rest]) -> [P | Rest];
stat_name_(N) -> N.

unreg_stats(P, App, Type, [Op, time], Index) ->
  unreg_from_both([P, App, Type, Op, time, Index]);
unreg_stats(P, App, Type, Mod, Index) ->
  unreg_from_both([P, App, Type, Mod, Index]).

unreg_from_both(StatName) ->
  riak_stat_coordinator:unregister(StatName).
