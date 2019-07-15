%%%-------------------------------------------------------------------
%%% @doc
%%% RPC calls from riak_core_console are directed to this module to
%%% enable/disable or read stats from exometer/metadata,
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
  reset_stat/1]).

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
  priority
}).

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
  {Reply, Attr} = gen_server:call(?SERVER, {show, Arg, Status}),
  print_stats(Reply, Attr).

-spec(show_stat_0(data()) -> value()).
%% @doc
%% Check which stats in exometer are not updating, only checks enabled
%% @end
show_stat_0(Arg) ->
  NotUpdating = gen_server:call(?SERVER, {show_stat_0, Arg}),
  print(NotUpdating).

-spec(stat_info(data()) -> value()).
%% @doc
%% Returns all the stats information
%% @end
stat_info(Arg) ->
  {Attrs, RestArg} = gen_server:call(?SERVER, {info_stat, Arg}),
  [print_stats(E, Attrs) || {{E, _S, _S}, _DP} <- find_entries(RestArg, '_')].

-spec(disable_stat_0(data()) -> value()).
%% @doc
%% Similar to the function above, but will disable all the stats that
%% are not updating
%% @end
disable_stat_0(Arg) ->
  gen_server:call(?SERVER, {disable_stat_0, Arg}).

-spec(status_change(data(), status()) -> value()).
%% @doc
%% change the status of the stat in metadata and in exometer
%% @end
status_change(Arg, ToStatus) ->
  CleanStats = clean_stats(Arg, ToStatus),
  ToChange = gen_server:call(?SERVER, {change_status, CleanStats, ToStatus}),
  responder(ToChange, fun change_status/2, ToStatus).

-spec(reset_stat(data()) -> value()).
%% @doc
%% resets the stats in metadata and exometer and tells metadata that the stat
%% has been reset
%% @end
reset_stat(Arg) ->
  Stats = [Stat || {Stat, _Info} <- clean_stats(Arg, '_')],
  responder(Stats, fun reset_stats/2, enabled).

%%%===================================================================
%%% Admin API
%%%===================================================================

clean_stats(Arg, Status) ->
  riak_stat_admin:parse_information(Arg, Status).

print_stats(Entries, Attributes) ->
  riak_stat_admin:print(Entries, Attributes).

find_entries(Stats, Status) ->
  riak_stat_data:find_entries(Stats, Status).

%%%===================================================================
%%% Coordinator API
%%%===================================================================

change_status(Name, ToStatus) ->
  riak_stat_coordinator:change_status([{Name, {status, ToStatus}}]).

reset_stats(Name, _status) ->
  reset_stats(Name).
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
  P = riak_stat_admin:priority(),
  {ok, #state{priority = P}}.

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
  {reply, {Reply, []}, State};
handle_call({info_stat, Arg}, _From, State) ->
  {reply, pick_info_attrs(split_arg(Arg)), State};
handle_call({change_status, CleanStats, _ToStatus}, _From, State) ->
  ToChange =
  lists:map(fun({Stat, _Info}) -> Stat end, CleanStats),
    {reply, ToChange, State};
handle_call({show_stat_0, Arg}, _From, State) ->
  io:fwrite("Arg: ~p~n", [Arg]),
  Reply = not_updating(Arg),
  {reply, Reply, State};
handle_call({disable_stat_0, Arg}, _From, State) ->
  NotUpdating = not_updating(Arg),
  [status_change(Name, disabled) || {{Name, _Val}, _L} <- NotUpdating],
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
%%% Internal functions
%%%===================================================================

print(Response) ->
  lists:map(fun({R, A}) ->
    riak_stat_info:print(R, A)
            end, Response).

not_updating(Arg) ->
  Stats = find_entries(Arg, enabled),
  Entries =
  lists:foldl(fun({{E, _S, _F}, _DP}, Acc) ->
                  case riak_stat_coordinator:get_info(E, value) of
                    [] ->
                      [{{E, 0}, []} | Acc];
                    _ -> Acc
                  end
              end, [], Stats),
  riak_stat_admin:print(Entries).


% Extra in the case of change status is the new status
% in the case of reset status it is - the incremental value ot reset it by
responder(Tochange, Fun, Status) ->
  lists:foreach(
    fun([]) ->
      io:fwrite(
        "No matching stats~n");
      (N) ->
        io:fwrite("~p~n", [Fun(N, Status)])
    end, Tochange).

split_arg([Str]) ->
  re:split(Str, "\\s", [{return, list}]).

pick_info_attrs(Arg) ->
  riak_stat_info:pick_info_attrs(Arg).