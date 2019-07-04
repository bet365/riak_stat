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

%% API
-export([
  show_stat/2, show_stat_0/1, stat_info/1,
  disable_stat_0/1, status_change/2, reset_stat/1]).

%% Admin API
-export([]).

%% Coordinator API
-export([]).

%% Cache API
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
  priority = metadata
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(show_stat(Arg :: term(), Status :: atom()) -> term()).
%% @doc
%% Show enabled or disabled stats
%% when using riak-admin stat show riak.** enabled stats will show by default
%%
%% otherwise use: riak-admin stat show-enabled | show-disabled
%% @end
show_stat(Arg, Status) ->

  Reply = gen_server:call(?SERVER, {show, Arg, Status}),
  print_stats(Reply, []).

-spec(show_stat_0(Arg :: term()) -> term()).
%% @doc
%% Check which stats in exometer are not updating, only checks enabled
%% @end
show_stat_0(Arg) ->
  NotUpdating = gen_server:call(?SERVER, {show_stat_0, Arg}),
  print(NotUpdating).

-spec(stat_info(Arg :: term()) -> term()).
%% @doc
%% Returns all the stats information
%% @end
stat_info(Arg) ->
  {Attrs, RestArg} = gen_server:call(?SERVER, {info_stat, Arg}),
  [print_stats(E, Attrs) || E <- find_entries(RestArg, '_')].

-spec(disable_stat_0(Arg :: term()) -> term()).
%% @doc
%% Similar to the function above, but will disable all the stats that
%% are not updating
%% @end
disable_stat_0(Arg) ->
  gen_server:call(?SERVER, {disable_stat_0, Arg}).

-spec(status_change(Arg :: term(), ToStatus :: atom()) -> term()).
%% @doc
%% change the status of the stat in metadata and in exometer
%% @end
status_change(Arg, ToStatus) ->
  % todo: get the list of stats and the status pulled out of the metadata
  OldStatus = gen_server:call(?SERVER, {change_status, Arg, ToStatus}),
  responder(Arg, OldStatus, fun change_status/2, ToStatus).

-spec(reset_stat(Arg :: term()) -> term()).
%% @doc
%% resets the stats in metadata and exometer and tells metadata that the stat
%% has been reset
%% @end
reset_stat(Arg) ->
%%  gen_server:call(?SERVER, {reset, Arg}).
  responder(Arg, enabled, fun reset_stats/2, []).

%%%===================================================================
%%% Admin API
%%%===================================================================

print_stats(Entries, Attributes) ->
  riak_stat_admin:print(Entries, Attributes).


%%%===================================================================
%%% Coordinator API
%%%===================================================================

change_status(Name, ToStatus) ->
  riak_stat_coordinator:coordinate(change_status, [{Name, {status, ToStatus}}]).
%%  case change_meta_status(Name, ToStatus) of
%%    ok ->
%%      change_exom_status(Name, ToStatus);
%%    {error, Reason} ->
%%      Reason;
%%    _ ->
%%      change_exom_status(Name, ToStatus)
%%  end.


reset_stats(Name, []) ->
  reset_stats(Name).
reset_stats(Name) ->
  riak_stat_coordinator:reset_stats(Name).
%%case reset_meta_stat(Name) of
%%ok ->
%%reset_exom_stat(Name);
%%_ ->
%%io:fwrite("Could not reset stat ~p~n", [Name])
%%end.


%%%===================================================================
%%% Cache API
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
  %% todo: check in cache first
  Reply = check_in(Priority, Arg),
  {reply, {Reply, []}, State};
handle_call({info_stat, Arg}, _From, State) ->
  {reply, pick_info_attrs(split_arg(Arg)), State};
handle_call({change_status, _Arg, ToStatus}, _From, State) ->
  OldStatus = case ToStatus of
                enabled -> disabled;
                disabled -> enabled;
                _ -> '_'
              end,

  {reply, OldStatus, State};
handle_call({show_stat_0, Arg}, _From, State) ->
  io:fwrite("Arg: ~p~n", [Arg]),
  not_updating(Arg),
  {reply, ok, State};
handle_call({disable_stat_0, Arg}, _From, State) ->
  {NotUpdating, _Updating} = not_updating(Arg),
  [status_change(Name, disabled) || {Name, _Val} <- NotUpdating],
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
  find_meta_stat_status(Arg);
check_in(exometer, Arg) ->
  find_exom_stat_status(Arg).

find_meta_stat_status(Arg) ->
  % todo: metadata is the default
  riak_stat_coordinator:metadata(show_stat, Arg, [status]).

find_exom_stat_status(Arg) ->
  riak_stat_coordinator:exometer(show_stat, Arg, [status]).


not_updating(Arg) ->
  {{Entries, _, _S}, _DPs} = find_entries(Arg, enabled),
  riak_stat_assist_mgr:print_stats0(Entries).

find_entries(Arg, Status) ->
  riak_stat_admin:find_entries(Arg, Status).

% Extra in the case of change status is the new status
% in the case of reset status it is - the incremental value ot reset it by
responder(Arg, CuStatus, Fun, Extra) ->
  lists:foreach(
    fun({[{LP, []}], _}) ->
      io:fwrite(
        "== ~s (Legacy pattern): No matching stats ==~n", [LP]);
      ({[{LP, Matches}], _}) ->
        io:fwrite("== ~s (Legacy pattern): ==~n", [LP]),
        [io:fwrite("~p: ~p~n", [N, Fun(N, Extra)])
          || {N, _} <- Matches];
      ({Entries, _}) ->
        [io:fwrite("~p: ~p~n", [N, Fun(N, Extra)])
          || {N, _, _} <- Entries]
    end, find_entries(Arg, CuStatus)).

split_arg([Str]) ->
  re:split(Str, "\\s", [{return, list}]).

pick_info_attrs(Arg) ->
  case lists:foldr(
    fun("-name", {As, Ps}) -> {[name | As], Ps};
      ("-type", {As, Ps}) -> {[type | As], Ps};
      ("-module", {As, Ps}) -> {[module | As], Ps};
      ("-value", {As, Ps}) -> {[value | As], Ps};
      ("-cache", {As, Ps}) -> {[cache | As], Ps};
      ("-status", {As, Ps}) -> {[status | As], Ps};
      ("-timestamp", {As, Ps}) -> {[timestamp | As], Ps};
      ("-options", {As, Ps}) -> {[options | As], Ps};
      (P, {As, Ps}) -> {As, [P | Ps]}
    end, {[], []}, Arg) of
    {[], Rest} ->
      {[name, type, module, value, cache, status, timestamp, options], Rest};
    Other ->
      Other
  end.