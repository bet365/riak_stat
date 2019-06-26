%%%-------------------------------------------------------------------
%%% @author savannahallsop
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2019 14:40
%%%-------------------------------------------------------------------
-module(riak_stat_console).
-author("savannahallsop").

-behaviour(gen_server).

-export([coordinate/2]).
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

-record(state, {
  priority = metadata
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(coordinate(Fun :: term(), Arg :: term()) ->
  ok | {error, Reason :: term()} | term()).
%% @doc
%% coordinates the function calls from riak_core_console
%% the function is checked in riak_stat_data if it is a function that
%% actually exists, if not then {error, no_function_found} is returned
%% @end
coordinate(Fun, Arg) ->
  Fun1 = sanitise_func(Fun),
  Arg1 = sanitise_data(Arg),
  Response = gen_server:call(?SERVER, {Fun1, Arg1}),
  print(Response).


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
  {ok, #state{}}.

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
handle_call({stat_show, Arg}, _From, State = #state{priority = Priority}) ->
  Reply = check_in(Priority, Arg),
  {reply, {Reply, []}, State};
handle_call({stat_info, Arg}, _From, State) ->
  Arg,
  {reply, ok, State};
handle_call({stat_enable, Arg}, _From, State) ->
  Arg,
  {reply, ok, State};
handle_call({stat_disable, Arg}, _From, State) ->
  Arg,
  {reply, ok, State};
handle_call({stat_show_0, Arg}, _From, State) ->
  Arg,
  {reply, ok, State};
handle_call({stat_disable_0, Arg}, _From, State) ->
  Arg,
  {reply, ok, State};
handle_call({stat_reset, Arg}, _From, State) ->
  Arg,
  {reply, ok, State};
handle_call({stat_disabled, Arg}, _From, State) ->
  Arg,
  {reply, ok, State};

handle_call({Request, _Arg}, _From, State) ->
  {reply, {error, Request}, State};
handle_call(Request, _From, State) ->
  {reply, {error, Request}, State}.

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

print(Response) ->
  lists:map(fun({R, A}) ->
  riak_stat_info:print(R, A)
            end, Response).

sanitise_func(Fun) ->
  riak_stat_data:sanitise_func(Fun).

sanitise_data(Arg) ->
  riak_stat_data:sanitise_data(Arg).

%%%
%%check_in(metadata, {Stat, Status}) ->
%%  find_meta_info(Stat, Status);
%%check_in(exometer, {Stat, Status}) ->
%%  find_exom_info(Stat, Status);
check_in(metadata, Arg) ->
  find_meta_info(Arg);
check_in(exometer, Arg) ->
  find_exom_info(Arg).

find_meta_info(Arg) ->
  riak_stat_metadata:get(Arg, status).

find_exom_info(Arg) ->
  riak_stat_exometer:find_entries(Arg).


