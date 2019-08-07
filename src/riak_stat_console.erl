%%%-------------------------------------------------------------------
%%% @doc
%%% calls from riak_core_console are directed to this module to
%%% enable/disable or read stats from exometer/metadata
%%%
%%% calls from exoskeleskin point to this module to retrieve stats
%%% for a UDP or HTTP endpoint
%%% @end
%%%-------------------------------------------------------------------
-module(riak_stat_console).

-include("riak_stat.hrl").

%% API
-export([
    show_stat/1,
    show_stat_0/1,
    stat_info/1,
    disable_stat_0/1,
    status_change/2,
    reset_stat/1
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec(show_stat(data()) -> value()).
%% @doc
%% Show enabled or disabled stats
%% when using riak-admin stat show riak.** enabled stats will show by default
%%
%% otherwise use: riak-admin stat show <entry>/status=* | disabled
%% @end
show_stat(Arg) ->
    [{_S, MatchSpec, DP}] = data_sanitise(Arg),
    Entries = select_entries(MatchSpec),
    Stats =
        case DP of
            default -> [{Entry, Status} || {Entry, _, Status} <- Entries];
            _ -> [find_stat_info(Entry, DP) || {Entry, _, _} <- Entries]
        end,
    print_stats(Stats).

-spec(show_stat_0(data()) -> value()).
%% @doc
%% Check which stats in exometer are not updating, only checks enabled
%% @end
show_stat_0(Arg) ->
    [{_Stats, MatchSpec, _DP}] = data_sanitise(Arg),
    Entries = [Entry || {Entry, _,_} <- select_entries(MatchSpec)],
    NotUpdating = not_updating(Entries),
    print_stats(NotUpdating).

-spec(stat_info(data()) -> value()).
%% @doc
%% Returns all the stats information
%% @end
stat_info(Arg) ->
    {Attrs, RestArg} = pick_info_attrs(Arg),
    [{Stats, _MatchSpec, _DP}] = data_sanitise(RestArg),
    Entries = find_entries(Stats, enabled),
    Found = [find_stat_info(Entry, Attrs) || {Entry, _} <- Entries],
    print_stats(Found).

-spec(disable_stat_0(data()) -> ok).
%% @doc
%% Similar to the function above, but will disable all the stats that
%% are not updating
%% @end
disable_stat_0(Arg) ->
    [{_S, MatchSpec, _DP}] = data_sanitise(Arg),
    Entries = [Entry || {Entry, _,_} <- select_entries(MatchSpec)],
    NotUpdating = not_updating(Entries),
    DisableTheseStats =
        lists:map(fun({Name, _V}) ->
            {Name, {status, disabled}}
                  end, NotUpdating),
    change_status(DisableTheseStats).

-spec(status_change(data(), status()) -> ok).
%% @doc
%% change the status of the stat (in metadata and) in exometer
%% @end
status_change(Arg, ToStatus) ->
    [{Stats, _MatchSpec, _DP}] = data_sanitise(Arg),
    Entries = % if disabling lots of stats, pull out only enabled ones
    case ToStatus of
        enabled -> find_entries(Stats, disabled);
        disabled -> find_entries(Stats, enabled)
    end,
    change_status([{Stat, {status, Status}} || {Stat, Status} <- Entries]).

-spec(reset_stat(data()) -> ok).
%% @doc
%% resets the stats in metadata and exometer and tells metadata that the stat
%% has been reset
%% @end
reset_stat(Arg) ->
    [{Stats, _MatchSpec, _DP}] = data_sanitise(Arg),
    reset_stats([Entry || {Entry, _} <- find_entries(Stats, enabled)]).

%%%===================================================================
%%% Admin API
%%%===================================================================

data_sanitise(Arg) ->
    riak_stat_admin:data_sanitise(Arg).

print_stats(Entries) ->
    print_stats(Entries, []).
print_stats(Entries, Attributes) ->
    riak_stat_admin:print(Entries, Attributes).

%%%===================================================================
%%% Coordinator API
%%%===================================================================

find_entries(StatNames, Status) ->
    riak_stat_coordinator:find_entries(StatNames, Status).

select_entries(MS) ->
    riak_stat_coordinator:select(MS).

not_updating(StatNames) ->
    riak_stat_coordinator:find_static_stats(StatNames).

find_stat_info(Stats, Info) ->
    riak_stat_coordinator:find_stats_info(Stats, Info).

change_status(Stats) ->
    riak_stat_coordinator:change_status(Stats).

reset_stats(Name) ->
    riak_stat_coordinator:reset_stat(Name).


%%%===================================================================
%%% Helper functions
%%%===================================================================

-spec(pick_info_attrs(data()) -> value()).
%% @doc get list of attrs to print @end
pick_info_attrs(Arg) ->
    case lists:foldr(
        fun ("-name", {As, Ps}) -> {[name | As], Ps};
            ("-type", {As, Ps}) -> {[type | As], Ps};
            ("-module", {As, Ps}) -> {[module | As], Ps};
            ("-value", {As, Ps}) -> {[value | As], Ps};
            ("-cache", {As, Ps}) -> {[cache | As], Ps};
            ("-status", {As, Ps}) -> {[status | As], Ps};
            ("-timestamp", {As, Ps}) -> {[timestamp | As], Ps};
            ("-options", {As, Ps}) -> {[options | As], Ps};
            (P, {As, Ps}) -> {As, [P | Ps]}
        end, {[], []}, split_arg(Arg)) of
        {[], Rest} ->
            {[name, type, module, value, cache, status, timestamp, options], Rest};
        Other ->
            Other
    end.

split_arg([Str]) ->
    re:split(Str, "\\s", [{return, list}]).