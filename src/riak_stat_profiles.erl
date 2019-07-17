%%%-------------------------------------------------------------------
%%% @doc
%%% Depends on riak_stat_admin which transforms the data inputted to
%%% a format which works for metadata and exometer.
%%% It sends data to exometer or metadata via riak_stat_coordinator
%%%
%%% save-profile <entry> ->
%%%     Saves the current stats and their status as a value to the
%%%     key: <entry>, in the metadata
%%%
%%% load-profile <entry> ->
%%%     Loads a profile that is saved in the metadata with the key
%%%     <entry>, pulls the stats and their status out of the metadata
%%%     and sends to riak_stat_coordinator to change the statuses
%%%
%%% delete-profile <entry> ->
%%%     Delete a profile <entry> from the metadata, the metadata leaves
%%%     the profile with a tombstone
%%%
%%% reset-profile ->
%%%     unloads the current profile and changes all the stats back to
%%%     enabled, no entry needed
%%%
%%% pull_profiles() ->
%%%     Pulls the list of all the profiles saved in the metadata and
%%%     their stats.
%%% @end
%%%-------------------------------------------------------------------
-module(riak_stat_profiles).

-behaviour(gen_server).
-include("riak_stat.hrl").

%% API
-export([
    save_profile/1,
    load_profile/1,
    delete_profile/1,
    reset_profile/0,
    pull_profiles/0
]).

%% gen_server callbacks
-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
-define(TIME, vclock:fresh()).
-define(NODEID, riak_core_nodeid:get()).

-record(state, {
    profile = none,
    profilelist
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(save_profile(profilename()) -> response()).
%% @doc
%% Data comes in already formatted unless nothing is entered, then the
%% response is 'no_data'.
%% Saves the profile name in the metadata with all the current stats and
%% their status as the value
%% @end
save_profile(ProfileName) ->
    gen_server:call(?SERVER, {save, ProfileName}).

-spec(load_profile(profilename()) -> response()).
%% @doc
%% load a profile saved in the metadata, If the profile is not there it
%% will return {error, no_profile}, if nothing was entered it will return
%% the statement below. Stats will be pulled out of the metadata and
%% compared to the current status of the stats, It will only enable the
%% disabled and disable the enabled stats etc, any unregistered stats that
%% are saved in the profile will do nothing.
%% the return will be 'ok'
%% @end
load_profile(ProfileName) ->
    gen_server:call(?SERVER, {load, ProfileName}).

-spec(delete_profile(profilename()) -> response()).
%% @doc
%% deletes the profile from the metadata and all its values but it does
%% not affect the status of the stats, metadata does not remove an entry
%% but does replace the data with a tombstone
%% @end
delete_profile(ProfileName) ->
    gen_server:call(?SERVER, {delete, ProfileName}).

-spec(reset_profile() -> response()).
%% @doc
%% resets the profile so no profile is loaded and will enable all the
%% disabled stats. it returns a ok if everything went ok, or
%% {error, Reason}
%% @end
reset_profile() ->
    Reply = gen_server:call(?SERVER, reset),
    print(Reply).

%%%===================================================================
%%% Admin API
%%%===================================================================

print({error, Reason}) ->
    print(Reason);
print(Data) when Data == no_data; Data == no_stats; Data == no_profile ->
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
    Profiles = pull_profiles(), % Pull out already saved profiles in the metadata
    Tid =                       % create ets for profiles
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
handle_call({_Fun, []}, _From, State) ->
    {reply, {error, no_data}, State};
handle_call({save, Arg}, _From, State = #state{profilelist = ProfileList}) ->
    ets:insert(ProfileList, {Arg, ?NODEID}),
    save_profile_(Arg),
    {reply, ok, State};
handle_call({load, Arg}, _From, State = #state{profilelist = ProfileList}) ->
    Reply =
      case ets:lookup(ProfileList, Arg) of
        []              -> {error, no_profile};
        {Name, _NodeId} -> load_profile_(Name)
      end,
    {reply, Reply, State};
handle_call({delete, Arg}, _From, State = #state{profilelist = ProfileList}) ->
    Reply =
      case ets:lookup(ProfileList, Arg) of
        []              -> {error, no_profile};
        {Name, _NodeId} -> delete_profile_(Name)
      end,
    {reply, Reply, State};
handle_call(reset, _From, State) ->
    Reply = reset_profile_(),
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

save_profile_(ProfileName) ->
    riak_stat_coordinator:save_profile(ProfileName).

load_profile_(ProfileName) ->
    riak_stat_coordinator:load_profile(ProfileName).

delete_profile_(ProfileName) ->
    riak_stat_coordinator:delete_profile(ProfileName).

reset_profile_() ->
    riak_stat_coordinator:reset_profile().

pull_profiles() ->
    riak_stat_coordinator:get_profiles().