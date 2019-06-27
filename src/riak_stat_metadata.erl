%%%-------------------------------------------------------------------
%%% @author savannahallsop
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2019 14:40
%%%-------------------------------------------------------------------
-module(riak_stat_metadata).
-author("savannahallsop").

%% Primary API
-export([get/2, put/4, delete/2]).

%% Secondary API
-export([coordinate/1]).

%% helper API
-export([]).

-define(PFX, riak_stat_mngr:prefix()).
-define(STAT, stats).
-define(PROF, profiles).
-define(NODEID, riak_core_nodeid:get()).



coordinate({Fun, Arg}) ->
  case Fun of
    register ->
      {StatName, Type, Opts, Aliases} = Arg,
      register_stat(StatName, Type, Opts, Aliases)
  end.

%%%%%%%%%%%% REGISTERING %%%%%%%%%%%%

-spec(register_stat(StatName :: metadata_key(), Type :: atom() | term(), Opts :: list(), Aliases :: term()) ->
  ok | term() | {error, Reason :: term()}).
%% @doc
%% Checks if the stat is already registered in the metadata, if not it
%% registers it, and pulls out the options for the status and sends it
%% back to go into exometer
%% @end
register_stat(StatName, Type, Opts, Aliases) ->
  case check_meta(StatName) of % check registration
    undefined -> % if not registered return default Opts
      re_register_stat(StatName, Type, [{vclock, vclock:fresh(?NODEID, 1)} | Opts], Aliases),
      Opts;
    {_Type, MetaOpts, _Aliases} -> % if registered
      find_register_status(Opts, MetaOpts);
%%      re_register_stat(StatName, Type, NewOpts, Aliases);
    _ ->
      lager:debug("riak_stat_meta_mgr:register_stat --
      Could not register the stat:~n{{~p,~p},~p,{~p,~p,~p}}~n",
        [?NODEID, ?STAT, StatName, Type, Opts, Aliases])
  end.

-spec(check_meta(StatName :: metadata_key()) -> ok | term()).
%% @doc
%% returns the value of the stat from the metadata
%% @end
check_meta(StatName) ->
  case get({?NODEID, ?STAT}, StatName) of
    undefined ->
      undefined;
    [] ->
      undefined;
    Value ->
      case find_unregister_status(StatName, Value) of
        true ->
          lager:debug("Stat is unregistered: ~p~n", [StatName]),
          unregistered;
        false ->
          Value
      end
  end.

find_unregister_status(_SN, {_T, Opts, _A}) ->
  case lists:keyfind(unregistered, 1, Opts) of
    false ->
%%      set_options(SN, {unregistered, false}),
      false;
    {unregistered, Bool} ->
      Bool
  end.

find_register_status(NewOpts, MetaOpts) ->
  case lists:keyfind(status, 1, MetaOpts) of
    false ->
      NewOpts;
    Status -> % {status, disabled}
      [Status | NewOpts]
  end.

re_register_stat(StatName, Type, Opts, Aliases) ->
  % {{NodeId, stats}, [riak, riak_kv, node, gets], {spiral, [{resets,1},{status,enabled}],...}}
  put({?NODEID, ?STAT}, StatName, {Type, Opts, Aliases}),
%%  lager:info("Stat registered in metadata: {{~p,~p},~p,{~p,~p,~p}}~n"),
  ok.














get(Prefix, Key) ->
  riak_core_metadata:get(Prefix, Key).

put(Prefix, Key, Value) ->
  put(Prefix, Key, Value, []).

put(Prefix, Key, Value, Opts) ->
  riak_core_metadata:put(Prefix, Key, Value, Opts).

delete(Prefix, Key) ->
  riak_core_metadata:delete(Prefix, Key).






%% register_stats -> exometer
%% pull out the options and send to exometer, checking isnt really necessary

%% make Primary functions calls to the metadata

%% secondary calls, calls necessary for specific funstions or data

%% helper functions