%%%-------------------------------------------------------------------
%%% @author savannahallsop
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jun 2019 15:08
%%%-------------------------------------------------------------------
-module(riak_stat_admin).
-author("savannahallsop").

-behaviour(gen_server).

-export([coordinate/1, coordinate/2]).

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

-record(state, {statstable}).

%%%===================================================================
%%% API
%%%===================================================================

coordinate({Fun, Arg}) ->
  coordinate(Fun, Arg).
coordinate(Fun, Arg) ->
  gen_server:cast(?SERVER, {Fun, Arg}).
%%  case Fun of
%%    register ->
%%      register(Arg);
%%    update ->
%%      update(Arg);
%%    read ->
%%      read(Arg)
%%  end.

%%no_function_found(_Info) ->
%%  {error, no_function_found}.

register(Arg) ->
  Arg.

update(Arg) ->
  Arg.

read(Arg) ->
  Arg.



%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  StatsTable = ets:new(admin, [
    set,
    protected,
    {keypos, 1},
    {write_concurrency,true},
    {read_concurrency, true}
  ]),
  %% No heir, so on startup this ets:table is created
   {ok, #state{statstable = StatsTable}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_cast({register, {App, Stats}}, State =#state{statstable = ETS}) ->
  lists:foreach(fun(Stat) ->
    register_stat(prefix(), App, Stat, ETS)
                end, Stats),
  {noreply, State};
handle_cast({read, Stat}, State) ->

  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

prefix() ->
  app_helper:get_env(riak_core, stat_prefix, riak).

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
  NewOpts = to_metadata(register, {StatName, Type, Opts, Aliases}),
  to_exometer(register, {StatName, Type, NewOpts, Aliases}).

% Put the prefix and App name in front of every stat name.
stat_name(P, App, N) when is_atom(N) ->
  stat_name_([P, App, N]);
stat_name(P, App, N) when is_list(N) ->
  stat_name_([P, App | N]).

stat_name_([P, [] | Rest]) -> [P | Rest];
stat_name_(N) -> N.

%%%%%% METADATA %%%%%%

to_metadata(Fun, Arg) ->
  riak_stat_coordinator:coordinate(metadata, {Fun, Arg}).




%%%%%% EXOMETER %%%%%%

to_exometer(Fun, Arg) ->
  riak_stat_coordinator:coordinate(exometer, {Fun, Arg}).