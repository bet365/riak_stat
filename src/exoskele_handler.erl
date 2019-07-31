%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(exoskele_handler).

%% API
%%-export([]).

-define(PLAIN_TEXT_CONTENT_TYPE,  <<"text/plain">>).
-define(JSON_CONTENT_TYPE,  <<"application/json">>).

-define(JSON_HEADER, [{<<"content-type">>, ?JSON_CONTENT_TYPE}]).
-define(PLAIN_TEXT_HEADER, [{<<"content-type">>, ?PLAIN_TEXT_CONTENT_TYPE}]).

-export([init/3, handle/2, terminate/3]).

%% TODO

% request from riak_stat_console the information needed and then turn into Json
% return it back to http or udp to send to end point

% convert from cowboy to wm

%%--------------------------------------------------------------------
%% @doc
%% init the handler
%% @end
%%--------------------------------------------------------------------
init(_Transport, Req, []) ->
  {ok, Req, undefined}.

%%--------------------------------------------------------------------
%% @doc
%% Handle the request
%% @end
%%--------------------------------------------------------------------
handle(Req, State) ->
  {Qs, Req1} = cowboy_req:qs(Req),
  QsVals = cow_qs:parse_qs(Qs),
  handle_request(lists:keyfind(<<"format">>, 1, QsVals), Req1, State).

%%--------------------------------------------------------------------
%% @doc
%% Handle the request
%% @end
%%--------------------------------------------------------------------
handle_request({<<"format">>,<<"json">>}, Req, State) ->
  handle_json_text(Req, State);
handle_request(_, Req, State) ->
  handle_plain_text(Req, State).


%%--------------------------------------------------------------------
%% @doc
%% Handle JSON request
%% @end
%%--------------------------------------------------------------------
handle_json_text(Req, State) ->
  {ok, Req1} = cowboy_req:chunked_reply(200, ?JSON_HEADER, Req),

  {Qs, Req2} = cowboy_req:qs(Req1),
  QsVals = cow_qs:parse_qs(Qs),
  ExcludedDataPointQsVals = proplists:lookup_all(<<"exclude_dp">>, QsVals),
  ExcludedDataPoints = [binary_to_integer_or_atom(Value) || {_Key, Value} <- ExcludedDataPointQsVals],

  Metrics = exo_stats:get_values(['_']),
  Json = exo_metrics_formatter:metrics_to_json(Metrics, ExcludedDataPoints),
  cowboy_req:chunk(Json, Req2),

  {ok, Req2, State}.

%%--------------------------------------------------------------------
%% @doc
%% Attempt to convert a binary to an integer and if that fails, convert
%% it to an atom
%% @end
%%--------------------------------------------------------------------
binary_to_integer_or_atom(Binary) ->
  case catch binary_to_integer(Binary) of
    Integer when is_integer(Integer) ->
      Integer;
    _NotInteger ->
      binary_to_existing_atom(Binary, latin1)
  end.


%%--------------------------------------------------------------------
%% @doc
%% Handle plain text
%% @end
%%--------------------------------------------------------------------
-spec handle_plain_text(Req :: cowboy_req:req(), State :: any()) -> {ok, Req :: cowboy_req:req(), State :: any()}.
handle_plain_text(Req, State) ->
  {ok, Req1} = cowboy_req:chunked_reply(200, ?PLAIN_TEXT_HEADER, Req),

  Metrics = exo_stats:get_values(['_']),

  lists:foreach(fun({Metric, DataPoints}) ->
    Acc = exo_metrics_formatter:metric_to_string(Metric, DataPoints),
    cowboy_req:chunk(Acc, Req1)
                end, Metrics),

  {ok, Req1, State}.

%%--------------------------------------------------------------------
%% @doc
%% Called on terminate of handler
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _Req, _State) ->
  ok.