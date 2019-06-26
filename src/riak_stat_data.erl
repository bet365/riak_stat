%%%-------------------------------------------------------------------
%%% @author savannahallsop
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2019 14:50
%%%-------------------------------------------------------------------
-module(riak_stat_data).
-author("savannahallsop").

%% API
-export([sanitise_data/1, sanitise_func/1]).


%% sanitise_data(Arg) ->
%%
%% behave in a similar way to the find_entries, where it will parse the
%% data into a list, the way it is stored in metadata and exometer.

sanitise_data(Arg) ->
  Arg.

sanitise_func(Func) when is_atom(Func) ->
  sanitise_func(atom_to_list(Func));
sanitise_func(Func) when is_list(Func) ->
  list_to_atom(lists:flatten(func_sanitiser(Func))).

func_sanitiser(Func) ->
  case lists:foldl(fun(RealFuns, TrueFun) ->
    case string:equal(RealFuns, Func) of
      true ->
        [Func | TrueFun];
      false ->
        TrueFun
    end
              end, [], functions()) of
    [] ->
      ["no_function_found"];
    Fun ->
      Fun
  end.

functions() ->
  [ % profiles
    "load_profile",
    "add_profile",
    "add_profile_stat",
    "remove_profile",
    "remove_profile_stat",
    "reset_profiles",
    "check_profile_stat",
    "change_profile_stat",
    % console
    "stat_show",
    "stat_info",
    "stat_enable",
    "stat_disable",
    "stat_show_0",
    "stat_disable_0",
    "stat_reset",
    "stat_disabled",
    % admin
    "register",
    "update",
    "read"
  ].