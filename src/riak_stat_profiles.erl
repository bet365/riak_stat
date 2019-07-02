%%%-------------------------------------------------------------------
%%% @author savannahallsop
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2019 15:58
%%%-------------------------------------------------------------------
-module(riak_stat_profiles).
-author("savannahallsop").

-behaviour(gen_server).

%% API
-export([]).

%% Admin Api
-export([]).

%% Coordinator API
-export([]).




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
  profile = none,
  profilelist =[]
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(coordinate(Fun :: term(), Arg :: term()) ->
  ok | {error, Reason :: term()} | term()).
%% @doc
%% coordinates the function calls from riak_core_console for profile
%% requests,
%% the function is checked in riak_stat_data if it is a function that
%% actually exists, if not then {error, no_function_found} is returned
%% @end
coordinate(Fun, Arg) ->
  Fun1 = riak_stat_data:sanitise_func(Fun),
  gen_server:call(?SERVER, {Fun1, Arg}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  %% TODO: the list of profiles and their stats are passed in
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
  %% TODO: take the input for init and go through the profile status,
  %% if the status is enabled, then set the state{profile=thatprofile

  %% TODO:also send the list of stats to the metadata to check that the right
  %% stats are enabled

  %% TODO: save the list of profile in state{profilelist=List}

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
handle_call({load_profile, Arg}, _From, State) ->
  Arg,
  {reply, ok, State};
handle_call({add_profile, Arg}, _From, State) ->
  Arg,
  {reply, ok, State};
handle_call({add_profile_stat, Arg}, _From, State) ->
  Arg,
  {reply, ok, State};
handle_call({remove_profile, Arg}, _From, State) ->
  Arg,
  {reply, ok, State};
handle_call({remove_profile_stat, Arg}, _From, State) ->
  Arg,
  {reply, ok, State};
handle_call({reset_profile, Arg}, _From, State) ->
  Arg,
  {reply, ok, State};
handle_call({check_profile_stat, Arg}, _From, State) ->
  Arg,
  {reply, ok, State};
handle_call({change_profile_stat, Arg}, _From, State) ->
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
