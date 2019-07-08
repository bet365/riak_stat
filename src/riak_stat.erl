%%%-------------------------------------------------------------------
%%% @doc
%%% Core module of the riak_stat app, all functions from the
%%%riak_core_console and stat modules use these functions to perform
%%% the specific stats request needed.
%%%
%%% This module calls into riak_stat_admin, riak_stat_console and
%%% riak_stat_profile depending on what is needed
%%% @end
%%% Created : 01. Jul 2019 14:54
%%%-------------------------------------------------------------------
-module(riak_stat).

%% Console API
-export([
  show_stat_status/2, show_static_stats/1, show_stat_info/1,
  disable_stat_0/1, reset_stat/1,
  change_stat_status/2]).


%% Profile API
-export([
  save_current_profile/1,
  load_profile/1,
  delete_profile/1,
  reset_stats_and_profile/0]).

%% Externally needed API
-export([prefix/0, set_priority/1]).


%% API
-export([
  register/2,
  get_app_stats/1,
  update/3,
  unregister/4, unregister/1]).

-type arg()     :: any().
-type status()  :: atom().
-type print()   :: any().
-type app()     :: atom().
-type stats()   :: list() | tuple().
-type stat()    :: list() | atom().
-type type()    :: atom() | tuple().
-type reason()  :: any().
-type error()   :: {error, reason()}.

%%%===================================================================
%%% Common API
%%%===================================================================

prefix() ->
  app_helper:get_env(riak_stat, stat_prefix, riak).

%%%===================================================================
%%% Console API
%%%===================================================================

-spec(set_priority(arg()) -> ok).
set_priority(Priority) when is_atom(Priority) ->
  riak_stat_admin:set_priority(Priority);
set_priority(Priority) when is_list(Priority) ->
  set_priority(list_to_atom(Priority));
set_priority(Priority) when is_binary(Priority) ->
  set_priority(binary_to_existing_atom(Priority, latin1)).


-spec(show_stat_status(arg(), status()) -> ok | print()).
%% @doc
%% A call of riak-admin stat show-enabled | show-disabled <entry>
%% points to this function, it will by default go to metadata unless it
%% is changed to exometer with set_default(Def)...
%% It just returns a status or statuses of the entry or entries
%% provided.
%% @end
show_stat_status(Arg, Status) ->
  riak_stat_console:show_stat(Arg, Status).

-spec(show_static_stats(arg()) -> ok | print()).
%% @doc
%% A call from riak_core_console made through riak-admin of:
%% riak-admin stat show-0 <entry> -> which returns the stats in
%% exometer which have a value that is not updating.
%% It will show which stats that are not being used.
%%
%% %% in exometer if it is not updating it will have a value of 0
%% or []
%% @end
show_static_stats(Arg) ->
  riak_stat_console:show_stat_0(Arg).

-spec(disable_stat_0(arg()) -> ok | print()).
%% @doc
%% like the function above it will find the stats in exometer that are not
%% updating but will in turn disable them in both the metadata and in
%% exometer so the change is persisted
%% @end
disable_stat_0(Arg) ->
  riak_stat_console:disable_stat_0(Arg).

-spec(show_stat_info(arg()) -> ok | print()).
%% @doc
%% show the information that is kept in the metadata and in exometer for the stats
%% given
%% If the default is not metadata it will just return the information stored in
%% exometer
%% @end
show_stat_info(Arg) ->
  riak_stat_console:stat_info(Arg).

-spec(change_stat_status(arg(), status()) -> ok | print()).
%% @doc
%% change the status of the stat in metadata and exometer, unless the default
%% is not metadata and is set to exometer then the data goes to exometer
%% only.
%% @end
change_stat_status(Arg, ToStatus) ->
  riak_stat_console:status_change(Arg, ToStatus).

-spec(reset_stat(arg()) -> ok | term()).
%% @doc
%% reset the stat in the metadata and in exometer, both the metadata
%% and exometer keep history of the number of resets, except with the
%% metadata the number of resets is persisted
%% @end
reset_stat(Arg) ->
  riak_stat_console:reset_stat(Arg).


%%%===================================================================
%%% Admin API
%%%===================================================================

-spec(register(app(), stats()) -> ok).
%% @doc
%% register the stats stored in the _stat modules in both the metadata
%% and in exometer_core
%% @end
register(App, Stats) ->
  riak_stat_admin:register(prefix(), App, Stats).

-spec(get_app_stats(app()) -> ok | stats()).
%% @doc
%% pulls the list of stats out of riak_stat_admin for that app.
%% @end
get_app_stats(App) ->
  riak_stat_admin:read_stats(App).

-spec(update(stat(), non_neg_integer(), type()) -> ok | arg()).
update(Name, IncrBy, Type) ->
  riak_stat_admin:update(Name, IncrBy, Type).


-spec(unregister(arg()) -> ok | print()).
unregister({Mod, Idx, Type, App}) ->
  unregister(Mod, Idx, Type, App).

unregister(Mod, Idx, Type, App) ->
  riak_stat_admin:unregister(prefix(), App ,Mod, Idx, Type).


%%%===================================================================
%%% Profile API
%%%===================================================================

-spec(save_current_profile(arg()) -> ok | error()).
%% @doc
%% Pull out the current stats status, and store the profile_name and
%% list of stats into the metadata.
%% All unregistered stats are stored as {status, unregistered}, but kept
%% within the profile in case it becomes re_registered, it is defaulted to
%% disabled.
%% @end
save_current_profile(Arg) ->
  riak_stat_profiles:save_profile(Arg).

-spec(load_profile(arg()) -> ok | error()).
%% @doc
%% load a profile saved in the metadata, if the profile does not exist then
%% {error, no_profile} is returned.
%% @end
load_profile(Arg) ->
  riak_stat_profiles:load_profile(Arg).

-spec(delete_profile(arg()) -> ok | error()).
%% @doc
%% Deletes the profile in the metadata but does not reset the stats, that can
%% be done manually with the function reset_profile, this just removes the
%% snapshot of the stats from the metadata
%% @end
delete_profile(Arg) ->
  riak_stat_profiles:delete_profile(Arg).

-spec(reset_stats_and_profile() -> ok | error()).
%% @doc
%% Resets all disabled stats back to enabled. If the stat has a status of
%% {status, unregistered} then the stat is left as unregistered or disabled.
%% if it has been enabled since the profile was loaded then it will not be affected.
%% @end
reset_stats_and_profile() ->
  riak_stat_profiles:reset_profile().
