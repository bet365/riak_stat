%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(riak_stat_profiles).

-behaviour(gen_server).

%% API
-export([
  save_profile/1,
  load_profile/1,
  delete_profile/1,
  reset_profile/0
]).

%% Coordinator API
-export([
  save_profile_in/1,
  load_profile_in/1,
  delete_profile_in/1,
  reset_profile_in/0]).


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
-define(TIME, vclock:fresh()).

-record(state, {
  profile = none,
  profilelist
}).

-type data()      :: term().
-type reason()    :: any().
-type error()     :: {error, reason()}.
-type response()  :: ok | term() | error().

%%%===================================================================
%%% API
%%%===================================================================

-spec(save_profile(data()) -> response()).
%% @doc
%% Data comes in already sanitised so it is always an atom, unless
%% nothing is entered, then the response is 'no_data'
%% Saves the profile name in the metadata will all the current stats and
%% their status as the value
%% @end
save_profile(ProfileName) ->
  Name = parse_profile_name(ProfileName),
  gen_server:call(?SERVER, {save, Name}).

-spec(load_profile(data()) -> response()).
%% @doc
%% load a profile saved in the metadata, If the profile is not there it
%% will return {error, no_profile}, if nothing was entered it will return
%% the statement below. Stats will be pulled out of the metadata and
%% compared to the current status of the stats, It will only enabled the
%% disabled and disable the enabled stats etc, any unregistered stats that
%% are saved in the profile will do nothing.
%% the return will be the stats that were enabled, and any unregistered stats
%% that were saved
%% @end
load_profile(ProfileName) ->
  Name = parse_profile_name(ProfileName),
  Reply = gen_server:call(?SERVER, {load, Name}),
  print(Reply).

-spec(delete_profile(data()) -> response()).
%% @doc
%% deletes the profile from the metadata and all its values but it does
%% not affect the status of the stats, metadata does not remove an entry
%% but does replace the data with a tombstone
%% @end
delete_profile(ProfileName) ->
  Name = parse_profile_name(ProfileName),
  gen_server:call(?SERVER, {delete, Name}).

-spec(reset_profile() -> response()).
%% @doc
%% resets the profile so no profile is loaded and will enable all the
%% disabled stats. it returns a ok if everything went ok, or
%% {error, Reason} in case a stat breaks or a stat that is already enabled
%% gets enabled again for example.
reset_profile() ->
  Reply = gen_server:call(?SERVER, reset),
  print(Reply).

%%%===================================================================
%%% Admin API
%%%===================================================================

parse_profile_name([]) ->
  no_data;
parse_profile_name(ProfileName) ->
  NameIsInHere =
    riak_stat_data:parse_info(ProfileName, []),
  New = [Name || {Name, _EP, _Stuff} <- NameIsInHere],
  [Nam | _E] = New, Nam.

print({error, Reason}) ->
  print(Reason);
print(Data) when Data == no_data; Data == no_stats ->
  io:fwrite("No data found~n");
print(Stats) ->
  riak_stat_admin:print(Stats, []).

%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  Profiles = pull_profiles(),
  Tid =                    % create ets for profiles
    ets:new(profiles, [
      set,
      protected,
      {keypos, 1},
      {write_concurrency, true},
      {read_concurrency, true}
    ]),
  ets:insert(Tid, Profiles),
  {ok, #state{profilelist = Tid}}.

%%--------------------------------------------------------------------

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({_Fun, no_data}, _From, State) ->
  {reply, {error, no_data}, State};
handle_call({save, Arg}, _From, State = #state{profilelist = ProfileList}) ->
  NewProfList = lists:keystore(Arg, 1, ProfileList, {Arg, ?TIME}),
  save_profile_in(Arg),
  {reply, ok, State#state{profilelist = NewProfList}};
handle_call({load, Arg}, _From, State = #state{profilelist = ProfileList}) ->
  {Reply, NewState} =
    case lists:keymember(Arg, 1, ProfileList) of
      false ->
        {[], State};
      true ->
        {load_profile_in(Arg),
          State#state{profile = Arg}}
    end,
  {reply, Reply, NewState};
handle_call({delete, Arg}, _From, State = #state{profilelist = ProfileList}) ->
  {Reply, NewState} =
    case lists:keymember(Arg, 1, ProfileList) of
      false ->
        {[], State};
      true ->
        {delete_profile_in(Arg),
          State#state{
            profilelist = lists:keydelete(Arg, 1, ProfileList)}}
    end,
  {reply, Reply, NewState};
handle_call(reset, _From, State) ->
  Reply = reset_profile_in(),
  NewState = State#state{profile = none},
  {reply, Reply, NewState};
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
%%% Coordinator API
%%%===================================================================

save_profile_in(ProfileName) ->
  riak_stat_coordinator:save_profile(ProfileName).

load_profile_in(ProfileName) ->
  riak_stat_coordinator:load_profile(ProfileName).

delete_profile_in(ProfileName) ->
  riak_stat_coordinator:delete_profile(ProfileName).

reset_profile_in() ->
  riak_stat_coordinator:reset_profile().

pull_profiles() ->
  riak_stat_coordinator:get_profiles().