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
-export([
  save_profile/1,
  load_profile/1,
  delete_profile/1,
  reset_profile/0
]).

%% Admin Api
-export([print_stats/1]).

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

%%%===================================================================
%%% API
%%%===================================================================

-spec(save_profile(Data :: atom()) ->
  Response :: term() | {error, Reason :: term()}).
%% @doc
%% Data comes in already sanitised so it is always an atom, unless
%% nothing is entered, then the response is 'no_data'
%% Saves the profile name in the metadata will all the current stats and
%% their status as the value
%% @end
save_profile(ProfileName) ->
  Name = parse_profile_name(ProfileName),
  gen_server:call(?SERVER, {save, Name}),
  io:fwrite("~p saved~n", [ProfileName]).

-spec(load_profile(Data :: atom()) ->
  Response :: term() | {error, Reason :: term()}).
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

-spec(delete_profile(Data :: atom()) ->
  Response :: term() | {error, Reason :: term()}).
%% @doc
%% deletes the profile from the metadata and all its values but it does
%% not affect the status of the stats, metadata does not remove an entry
%% but does replace the data with a tombstone
%% @end
delete_profile(ProfileName) ->
  Name = parse_profile_name(ProfileName),
  gen_server:call(?SERVER, {delete, Name}).

-spec(reset_profile() -> term() | ok | {error, Reason :: term()}).
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
  {Name, _EP, _Stuff} =
    riak_stat_admin:parse_information(ProfileName, []),
  Name.

print({error, Reason}) ->
  print(Reason);
print(no_data) ->
  io:fwrite("No data found~n");
print(Stats) ->
  riak_stat_admin:print(Stats, []).

%%%===================================================================
%%% Coordinator API
%%%===================================================================

save_profile_in(ProfileName) ->
  riak_stat_coordinator:coordinate(save_profile, ProfileName).

load_profile_in(ProfileName) ->
  riak_stat_coordinator:coordinate(load_profile, ProfileName).

delete_profile_in(ProfileName) ->
  riak_stat_coordinator:coordinate(delete_profile, ProfileName).

reset_profile_in() ->
  riak_stat_coordinator:coordinate(reset_profile).

pull_profiles() ->
  riak_stat_coordinator:coordinate(pull_profiles).

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
%%  Stats = pull_profiles(),
%%  Tid =                    % create ets for profiles
%%    ets:new(profiles, [
%%      set,
%%      protected,
%%      {keypos, 1},
%%      {write_concurrency, true},
%%      {read_concurrency, true}
%%    ]),
%%  ets:insert(Tid, Stats),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------

%% TOdo: change the profile list to an ets?
%% instead of a list.
%% or pull the profiles out of metadata and store in this list, however this seems
%% mostly pointless as it can be just done on metadata end.
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
  Reply = save_profile_in(Arg),
  {reply, Reply, State#state{profilelist = NewProfList}};
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
