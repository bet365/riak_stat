%%%-------------------------------------------------------------------
%%% @doc
%%% Parsing data for profiles and console commands to the format used
%%% in the metadata and exometer.
%%%
%%% Console and profile commands input:
%%% [<<"riak.riak_kv.**">>] and [<<"test-profile-name">>]
%%%
%%% Additional Exoskeleskin input data:
%%% [<<"udp.8080">>]
%%%
%%% Parse the Data into an atom or list format.
%%% @end
%%%-------------------------------------------------------------------
-module(riak_stat_data).

-include("riak_stat.hrl").

-export([
    data_sanitise/1,
    data_sanitise/3
]).

-spec(data_sanitise(data()) -> data()).
%% @doc
%% this is for data coming in from the console of format [<<"X">>],
%% this one transforms the arg given into usable arguments in admin
%% console and metadata. Taken from legacy code 2013-2014
%% @end
data_sanitise(Data) ->
    data_sanitise(Data, '_', enabled).
data_sanitise([Da | Ta], Type, Status) when is_list(Da) ->
    Dat = list_to_binary(Da),
    A = lists:map(fun(Stat) -> list_to_binary(Stat) end, Ta),
%%    Tat =
%%    try  [_T | A] = Ta of
%%        Tl when is_list(Tl) and A == [] ->
%%            list_to_binary(Tl);
%%        Tl when A == [] ->
%%            Tl;
%%        Tl ->
%%            L1 = list_to_binary(Tl),
%%            [Al] = A,
%%            L2 = list_to_binary(Al),
%%            [L1 | L2]
%%    catch error:_ ->
%%        Ta
%%    end,
    data_sanitise([Dat | A], Type, Status);
data_sanitise([Da | Ta], Type, Status) when is_atom(Da)  ->
    Data = lists:map(fun
                         ("**") -> <<"**">>;
                         ("*")  -> <<"*">>;
                         (D) -> atom_to_binary(D, latin1)
                     end, [Da|Ta]),
    data_sanitise(Data, Type, Status);
data_sanitise(Data, Type, Status) when is_binary(Data) ->
    data_sanitise([Data], Type, Status);
data_sanitise(Data, Type, Status) when is_atom(Data) ->
    data_sanitise([atom_to_binary(Data, latin1)], Type, Status);
data_sanitise(Data, Type, Status) when is_list(Data) ->
%%    io:format("Data in sanitise :~p~n", [Data]),
    lists:map(fun(D) ->
        data_sanitise_(D, Type, Status)
              end, Data).


data_sanitise_(Data, Type, Status) ->
    [Stat | Est] = re:split(Data, "/"),
    % [<<"riak.riak_kv.*.gets.**">> | <<"status=*">>]
    io:format("Data : ~p -> Stat : ~p, Est : ~p~n", [Data, Stat, Est]),
    {Type, Status, DPs} = type_status_and_dps(Est, Type, Status, default),
    io:format("Type : ~p -> Status : ~p, DPs : ~p~n", [Type, Status, DPs]),
    {Names, MatchSpecs} = stat_entries(Stat, Type, Status),
    io:format("Name : ~p, Matchspec : ~p, Dps : ~p~n", [Names, MatchSpecs, DPs]),
    {Names, MatchSpecs, DPs}.


%% @doc
%% when /status=*, /type=* or /datapoints is given it can be extracted out
%% @end
type_status_and_dps([<<"type=", T/binary>> | Rest], _Type, Status, DPs) ->
%%    io:format("types typestatusnaddps~n"),
    NewType =
    case T of
        <<"*">> -> '_';
        _ ->
            try binary_to_existing_atom(T, latin1)
            catch error:_ ->T
            end
    end, type_status_and_dps(Rest, NewType, Status, DPs);
type_status_and_dps([<<"status=", S/binary>> | Rest], Type, _Status, DPs) ->
%%    io:format("status typesatusanddps~n"),
    NewStatus =
    case S of
        <<"*">> -> '_';
        <<"enabled">> ->  enabled;
        <<"disabled">> -> disabled
    end, type_status_and_dps(Rest, Type, NewStatus, DPs);
type_status_and_dps([DPsBin | Rest], Type, Status, DPs) ->
%%    io:format("dps typestatusanddps~n"),
    NewDPs = merge(
        [binary_to_existing_atom(D, latin1) || D <- re:split(DPsBin, ",")],
        DPs),
    type_status_and_dps(Rest, Type, Status, NewDPs);
type_status_and_dps([], Type, Status, DPs) ->
%%    io:format("empty typestatusanddps~n"),
    {Type, Status, DPs}.

merge([_ | _] = DPs, default) ->
    DPs;
merge([H | T], DPs) ->
    case lists:member(H, DPs) of
        true -> merge(T, DPs);
        false -> merge(T, DPs ++ [H])
    end;
merge([], DPs) ->
    DPs.

%% @doc
%% Input is a name of a stat(s) in binary
%% output is the name of a stat or several in list form
%% [<<"riak,riak_kv,*.gets.**">>] -> [riak,riak_kv,'_',gets,'_','_',...]
%% @end
stat_entries([], Type, Status) ->
    {[],[{{[?PFX]++'_', Type, '_'}, [{'=:=', '$status', Status}], ['$_']}]};
stat_entries("*", Type, Status) ->
    stat_entries([], Type, Status);
stat_entries("[" ++ _ = Expr, _Type, _Status) ->
    case erl_scan:string(ensure_trailing_dot(Expr)) of
        {ok, Toks, _} ->
            case erl_parse:parse_exprs(Toks) of
                {ok, [Abst]} ->
                    partial_eval(Abst);
                Error ->
                    io:fwrite("(Parse error for ~p: ~p~n", [Expr, Error]),
                    []
            end;
        ScanErr ->
            io:fwrite("(Scan error for ~p: ~p~n", [Expr, ScanErr]),
            []
    end; %% legacy Code
stat_entries(Data, Type, Status) when is_atom(Status) ->
%%    io:format("stat_entries ----- Data : ~p ~n", [Data]),
    Parts = re:split(Data, "\\.", [{return, list}]),
    io:format("stat_entries -----Parts : ~p ~n", [Parts]),
    Heads = replace_parts(Parts),
    io:format("stat_entries -----Heads : ~p ~n", [Heads]),
    {Heads,[{{H, Type, Status}, [], ['$_']} || H <- Heads]};
stat_entries(_Stat, _Type, Status) ->
%%    io:format("stat_entries illegal~n"),
    io:fwrite("(Illegal status : ~p~n", [Status]).

replace_parts(Parts) ->
%%    io:format("replace parts: ~p~n", [Parts]),
    case split(Parts, "**", []) of
        {_, []} ->
            io:format("replace_parts {_,[]}~n"),

            [replace_parts_1(Parts)];
        {Before, After} ->
            io:format("replace_parts {Before,After}~n"),
            io:format("Before:~p, After:~p~n", [Before, After]),

            Head = replace_parts_1(Before),
            Tail = replace_parts_1(After),
            [Head ++ Pad ++ Tail || Pad <- pads()]
    end.

split([H | T], H, Acc) ->
    {lists:reverse(Acc), T};
split([H | T], X, Acc) ->
    split(T, X, [H | Acc]);
split([], _, Acc) ->
%%    io:format("split~n"),

    {lists:reverse(Acc), []}.


replace_parts_1([H | T]) ->
%%    io:format("replace parts again [~p | ~p]~n", [H,T]),
    R = replace_part(H),
    case T of
        '_' -> '_';
        "**" -> [R] ++ ['_'];
        ["**"] -> [R] ++ ['_'];
        _ -> [R | replace_parts_1(T)]
    end;
replace_parts_1([]) ->
    [].

replace_part(H) ->
    case H of
        '_' -> '_';
        "*" -> '_';
        "'" ++ _ ->
            case erl_scan:string(H) of
                {ok, [{atom, _, A}], _} ->
                    A;
                Error ->
                    lager:error("Cannot replace part: ~p~n", [Error])
            end;
        [C | _] when C >= $0, C =< $9 ->
            try list_to_integer(H)
            catch
                error:_ -> list_to_atom(H)
            end;
        _ -> list_to_atom(H)
    end.

pads() ->
    [lists:duplicate(N, '_') || N <- lists:seq(1,15)].

ensure_trailing_dot(Str) ->
    case lists:reverse(Str) of
        "." ++ _ ->
            Str;
        _ ->
            Str ++ "."
    end.

partial_eval({cons, _, H, T}) ->
    [partial_eval(H) | partial_eval(T)];
partial_eval({tuple, _, Elems}) ->
    list_to_tuple([partial_eval(E) || E <- Elems]);
partial_eval({op, _, '++', L1, L2}) ->
    partial_eval(L1) ++ partial_eval(L2);
partial_eval(X) ->
    erl_parse:normalise(X).
