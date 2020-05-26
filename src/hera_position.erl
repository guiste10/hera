%%%-------------------------------------------------------------------
%%% @author Julien Bastin <julien.bastin@student.uclouvain.be>
%%% @author Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% Module which includes all the functions related to our user case.
%%% Our user case consists in calculating the position of a move person in an empty room in realtime
%%% using GRiSP boards with Diligent pmod_maxsonar.
%%% @reference See <a href="https://grisp.org/" target="_blank">GRiSP site</a> and <a href="https://store.digilentinc.com/pmodmaxsonar-maxbotix-ultrasonic-range-finder/" target="_blank">Diligent site</a> for more information
%%% @end
%%% Created : 02. May 2020 2:22 AM
%%%-------------------------------------------------------------------

-module(hera_position).
-author("Julien Bastin <julien.bastin@student.uclouvain.be>, Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>").

-include("hera.hrl").

-export([launch_hera/4]).
-export([launch_hera/5]).
-export([launch_hera_shell/0]).
-export([restart_calculation/2]).
-export([restart_measurement/2]).
-export([restart/2]).
%%====================================================================
%% Macros
%%====================================================================

-define(SERVER, ?MODULE).

%%====================================================================
%% Records
%%====================================================================


%%%===================================================================
%%% API
%%%===================================================================

launch_hera(PosX, PosY, NodeId, DoFilter) ->
    Measurements = [
        {sonar, #{func => fun(InchToCm) -> sonar_measurement(InchToCm) end, args => [2.54], frequency => 100, 
        filtering => DoFilter, upperBound => 0.28,
         max_iterations => 400}},
        {pos, #{func => fun() -> {ok, #{x => PosX, y => PosY, node_id => NodeId}} end, args => [], frequency => 50, filtering => false, upperBound => 0.28, max_iterations => 3}}
    ],
    Calculations = [{position, #{func => fun(Id) -> calc_position(Id) end, args => [NodeId], frequency => 100, max_iterations => 400}}],
    hera:launch_app(Measurements, Calculations).

launch_hera(PosX, PosY, NodeId, Frequency, MaxIteration) ->
    Measurements = [
        {sonar, #{func => fun(InchToCm) -> sonar_measurement(InchToCm) end, args => [2.54], frequency => Frequency, 
        filtering => true, upperBound => 0.28,
        max_iterations => MaxIteration}},
        {pos, #{func => fun() -> {ok, #{x => PosX, y => PosY, node_id => NodeId}} end, args => [], frequency => 5000, filtering => false, upperBound => 0.28, max_iterations => 3}}
    ],
    Calculations = [{position, #{func => fun(Id) -> calc_position(Id) end, args => [NodeId], frequency => Frequency, max_iterations => MaxIteration}}],
    %Calculations = [], % no calculation
    hera:launch_app(Measurements, Calculations).

launch_hera_shell() ->
    hera:launch_app().

restart(Frequency, MaxIterations) ->
    restart_measurement(Frequency, MaxIterations),
    restart_calculation(Frequency, MaxIterations).

restart_calculation(Frequency, MaxIterations) ->
    hera:restart_calculation(position, Frequency, MaxIterations).

restart_measurement(Frequency, MaxIterations) ->
    hera:restart_measurement(sonar, Frequency, MaxIterations).

%%%===================================================================
%%% Internal functions
%%%===================================================================

sonar_measurement(InchToCm) ->
    case pmod_maxsonar:get() of
        undefined -> {error, "pmod_maxsonar not set up correctly"};
        Value -> {ok, Value*InchToCm}
    end.


calc_position(NodeId) ->
    case hera:get_data(sonar) of
        {error, Reason} ->
            logger:error(Reason),
            error;
        {ok, Sonar} ->
            case hera:get_data(pos) of
                {error, Reason} ->
                    logger:error(Reason),
                    error;
                {ok, Pos} ->
                    Nodes = lists:filter(fun(N) -> dict:is_key(N, Pos) end, dict:fetch_keys(Sonar)),
                    Values = [dict:fetch(Node, Sonar) || Node <- Nodes],
                    case Values of
                        [{_Seq1, R1}, {_Seq2, R2}] ->
                            [
                                {_, #{x := PosX1, y := PosY1, node_id := _NodeId1}},
                                {_, #{x := PosX2, y := PosY2, node_id := _NodeId2}}
                            ] = [dict:fetch(Node, Pos) || Node <- Nodes],
                            Separation = math:sqrt(math:pow(PosX2-PosX1, 2) + math:pow(PosY2-PosY1, 2)),
                            R1Sq = math : pow ( R1 , 2) ,
                            R2Sq = math : pow ( R2 , 2) ,
                            S2 = 2 * Separation ,
                            SSq = math : pow ( Separation , 2) ,
                            X = ( R1Sq - R2Sq + SSq ) / S2 ,
                            Helper = R1Sq - math : pow (X , 2),
                            if
                                Helper < 0 ->
                                    {error, "Position not definable: square root of neg number~n"};
                                true ->
                                    Y1 = math : sqrt ( Helper ) ,
                                    Y2 = - Y1,
                                    Result = io_lib:format("x1, ~.2f, y1, ~.2f, x2, ~.2f, y2, ~.2f", [X, Y1, X, Y2]),
                                    {ok, Result}
                            end;
                        [{_Seq1, V1}, {_Seq2, V2}, {_Seq3, V3}] ->
                            [
                                {_, #{x := PosX1, y := PosY1, node_id := _NodeId1}},
                                {_, #{x := PosX2, y := PosY2, node_id := _NodeId2}},
                                {_, #{x := PosX3, y := PosY3, node_id := _NodeId3}}
                            ] = [dict:fetch(Node, Pos) || Node <- Nodes],
                            {X_p, Y_p} = trilateration({V1, PosX1, PosY1}, {V2, PosX2, PosY2}, {V3, PosX3, PosY3}),
                            Result = io_lib:format("x, ~.2f, y, ~.2f", [X_p, Y_p]),
                            {ok, Result};
                        [{_, _}, {_, _}, {_, _}, {_, _}] ->
                            Neighbors = lists:filter(fun(N) -> neighbors(NodeId, dict:fetch(N, Pos)) end, Nodes),
                            [{_Seq1, V1}, {_Seq2, V2}, {_Seq3, V3}] = [dict:fetch(Node, Sonar) || Node <- Neighbors],
                            [
                                {_, #{x := PosX1, y := PosY1, node_id := _NodeId1}},
                                {_, #{x := PosX2, y := PosY2, node_id := _NodeId2}},
                                {_, #{x := PosX3, y := PosY3, node_id := _NodeId3}}
                            ] = [dict:fetch(Node, Pos) || Node <- Neighbors],
                            {X_p, Y_p} = trilateration({V1, PosX1, PosY1}, {V2, PosX2, PosY2}, {V3, PosX3, PosY3}),
                            Result = io_lib:format("x, ~.2f, y, ~.2f", [X_p, Y_p]),
                            {ok, Result};
                        _ ->
                            {error, "Not two mesurements available"}
                    end
            end
    end.

trilateration({V1, X1, Y1}, {V2, X2, Y2}, {V3, X3, Y3}) ->
    A = 2*X2 - 2*X1,
    B = 2*Y2 - 2*Y1,
    C = math:pow(V1, 2) - math:pow(V2, 2) - math:pow(X1, 2) + math:pow(X2, 2) - math:pow(Y1, 2) + math:pow(Y2, 2),
    D = 2*X3 - 2*X2,
    E = 2*Y3 - 2*Y2,
    F = math:pow(V2, 2) - math:pow(V3, 2) - math:pow(X2, 2) + math:pow(X3, 2) - math:pow(Y2, 2) + math:pow(Y3, 2),
    X_p = (C*E - F*B) / (E*A - B*D),
    Y_p = (C*D - A*F) / (B*D - A*E),
    {X_p, Y_p}.

neighbors(NodeId, {_, #{node_id := Id}}) ->
    if
        Id =:= NodeId -> true;
        Id =:= (NodeId + 4 + 1) rem 4 -> true;
        Id =:= (NodeId + 4 - 1) rem 4 -> true;
        true -> false
    end.