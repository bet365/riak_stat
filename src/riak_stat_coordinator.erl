%%%-------------------------------------------------------------------
%%% @doc
%%% The middleman between exometer and metadata and the rest of the app,
%%% any information needed from exometer or metadata goes through the
%%% coordinator
%%% @end
%%% Created : 25. Jun 2019 14:41
%%%-------------------------------------------------------------------
-module(riak_stat_coordinator).
-author("Savannah Allsop").

-include("riak_stat.hrl").

%% API
-export([
  get_priority/0,
  register/1,
  unregister/1,
  reset_stat/1,
  change_status/1,
  get_info/2, get_datapoint/2,
  select/1,
  alias/1, aliases/1,
  get_stat_info/1, get_app_stats/1,
  update/3, check_status/1
]).

%% Metadata API
-export([
  get_profiles/0,
  save_profile/1,
  load_profile/1,
  delete_profile/1,
  reset_profile/0]).

%% Exometer API
-export([
  read_cache/2,
  write_cache/3, write_cache/4,
  delete_cache/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec(get_priority() -> priority()).
%% @doc get the priority from riak_stat_admin @end
get_priority() ->
  riak_stat_admin:priority().

%%%%%%%%%%%%%%% Admin API %%%%%%%%%%%%%%%%%%%

-spec(register(data()) -> ok | error()).
%% @doc register in metadata and pull out the status,
%% and send that status to exometer @end
register({Stat, Type, _Opts, Aliases} = Arg) ->
  case register_in_metadata(Arg) of
    [] ->
      ok;
    NewOpts ->
      register_in_exometer(Stat, Type, NewOpts, Aliases)
  end.

-spec(unregister(statname()) -> ok | error()).
%% @doc set status to unregister in metadata, and delete
%% in exometer @end
unregister(StatName) ->
  unregister_in_metadata(StatName),
  unregister_in_exometer(StatName).

-spec(change_status(stats()) -> ok | error()).
%% @doc change status in metadata and then in exometer @end
change_status(StatsList) ->
  change_meta_status(StatsList),
  change_exom_status(StatsList).

-spec(update(statname(), incrvalue(), type()) -> ok).
%% @doc update unless disabled or unregistered @end
update(Name, Inc, Type) ->
  case check_in_meta(Name) of
    [] ->
      ok;
    unregistered ->
      ok;
    _ ->
      update_exom(Name, Inc, Type)
  end.

%%%===================================================================
%%% Exometer API
%%%===================================================================

get_info(Name, Info) ->
  riak_stat_exometer:info(Name, Info).

get_datapoint(Name, DP) ->
  riak_stat_exometer:get_datapoint(Name, DP).

select(Arg) ->
  riak_stat_exometer:select_stat(Arg).

alias(Arg) ->
  riak_stat_exometer:alias(Arg).

aliases({Arg, Value}) ->
  riak_stat_exometer:aliases(Arg, Value).


%%%%%%%%%%%%%%% Console API %%%%%%%%%%%%%%%%%%%

get_stat_info(Arg) ->
  riak_stat_exometer:get_value(Arg).

get_app_stats(Arg) ->
  case get_priority() of
    metadata ->
      [Stat || {Stat, {status, _Status}} <- get_current_meta_stats()];
    exometer ->
      get_stats(Arg)
  end.

reset_stat(StatName) ->
  reset_meta_stat(StatName),
  reset_exom_stat(StatName).

%%%===================================================================
%%% Metadata API
%%%===================================================================

check_status(Stat) ->
  riak_stat_metadata:check_status(Stat).

%% Api     %%

get_current_meta_stats() ->
  riak_stat_metadata:get_current_stats().

%% Admin   %%

register_in_metadata(StatInfo) ->
  riak_stat_metadata:register_stat(StatInfo).

unregister_in_metadata(StatName) ->
  riak_stat_metadata:unregister(StatName).

change_meta_status(Arg) ->
  riak_stat_metadata:change_status(Arg).

reset_meta_stat(Arg) ->
  riak_stat_metadata:reset_stat(Arg).

check_in_meta(Name) ->
  riak_stat_metadata:check_meta(Name).

%%%%%%%%%%%%%%% Profile API %%%%%%%%%%%%%%%%%%%

get_profiles() ->
  riak_stat_metadata:get_profiles().

save_profile(Profile) ->
  riak_stat_metadata:save_profile(Profile).

load_profile(Profile) ->
  riak_stat_metadata:load_profile(Profile).

delete_profile(Profile) ->
  riak_stat_metadata:delete_profile(Profile).

reset_profile() ->
  riak_stat_metadata:reset_profile().

%%%===================================================================
%%% Exometer API
%%%===================================================================

register_in_exometer(StatName, Type, Opts, Aliases) ->
  riak_stat_exometer:register_stat(StatName, Type, Opts, Aliases).

unregister_in_exometer(StatName) ->
  riak_stat_exometer:unregister_stat(StatName).

change_exom_status(Arg) ->
  riak_stat_exometer:change_status(Arg).

reset_exom_stat(Arg) ->
  riak_stat_exometer:reset_stat(Arg).

get_stats(Arg) ->
  riak_stat_exometer:read_stats(Arg).

update_exom(Name, IncrBy, Type) ->
  riak_stat_exometer:update_or_create(Name, IncrBy, Type).

%%%%%%%%% Caching %%%%%%%%%%% <- Unused

read_cache(Name, DP) ->
  riak_stat_exometer:read_cache(Name, DP).

write_cache(Name, DP, Value) ->
  write_cache(Name, DP, Value, undefined).

write_cache(Name, DP, Value, TTL) ->
  riak_stat_exometer:write_to_cache(Name, DP, Value, TTL).

delete_cache(Name, DP) ->
  riak_stat_exometer:delete_cache(Name, DP).