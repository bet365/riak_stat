-module(riak_stat_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(StartType, StartArgs) ->
    io:format("riak_stat_app:start(~p,~p)~n", [StartType, StartArgs]),
    riak_stat_sup:start_link(),
    io:format("riak_stat_sup:start_link()"),
    Started = riak_stat_config:ensure_all(),
    io:format("riak_stat_config:ensure_all()~nStarted: ~p~n", [Started]),
    exoskele_sup:start_link(),
    io:format("exoskele_sup:start_link()").

stop(_State) ->
    ok.
