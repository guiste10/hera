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

-export([launch_hera/3]).
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

launch_hera(Pos_x, Pos_y, Node_id) ->
    Measurements = [
        {sonar, #{func => fun(Inch_to_cm) -> sonar_measurement(Inch_to_cm) end, args => [2.54], frequency => 500, filtering => true, max_iterations => 300}},
        {pos, #{func => fun() -> {ok, #{x => Pos_x, y => Pos_y, node_id => Node_id}} end, args => [], frequency => 5000, filtering => false, max_iterations => 3}}
    ],
    Calculations = [{position, #{func => fun(Id) -> calc_position(Id) end, args => [Node_id], frequency => 500, max_iterations => 300}}],
    hera:launch_app(Measurements, Calculations).

launch_hera(Pos_x, Pos_y, Node_id, Frequency, Max_iteration) ->
    Measurements = [
        {sonar, #{func => fun(Inch_to_cm) -> sonar_measurement(Inch_to_cm) end, args => [2.54], frequency => Frequency, filtering => true, max_iterations => Max_iteration}},
        {pos, #{func => fun() -> {ok, #{x => Pos_x, y => Pos_y, node_id => Node_id}} end, args => [], frequency => 5000, filtering => false, max_iterations => 3}}
    ],
    Calculations = [{position, #{func => fun(Id) -> calc_position(Id) end, args => [Node_id], frequency => Frequency, max_iterations => Max_iteration}}],
    %Calculations = [],
    hera:launch_app(Measurements, Calculations).

launch_hera_shell() ->
    hera:launch_app().

restart(Frequency, Max_iterations) ->
    restart_measurement(Frequency, Max_iterations),
    restart_calculation(Frequency, Max_iterations).

restart_calculation(Frequency, Max_iterations) ->
    hera:restart_calculation(position, Frequency, Max_iterations).

restart_measurement(Frequency, Max_iterations) ->
    hera:restart_measurement(sonar, Frequency, Max_iterations).

%%%===================================================================
%%% Internal functions
%%%===================================================================

sonar_measurement(Inch_to_cm) ->
    case pmod_maxsonar:get() of
        undefined -> {error, "pmod_maxsonar not set up correctly"};
        Value -> {ok, Value*Inch_to_cm}
    end.

calc_position(Node_id) ->
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
                                {_, #{x := Pos_x1, y := Pos_y1, node_id := _Node_id1}},
                                {_, #{x := Pos_x2, y := Pos_y2, node_id := _Node_id2}}
                            ] = [dict:fetch(Node, Pos) || Node <- Nodes],
                            Separation = math:sqrt(math:pow(Pos_x2-Pos_x1, 2) + math:pow(Pos_y2-Pos_y1, 2)),
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
                                {_, #{x := Pos_x1, y := Pos_y1, node_id := _Node_id1}},
                                {_, #{x := Pos_x2, y := Pos_y2, node_id := _Node_id2}},
                                {_, #{x := Pos_x3, y := Pos_y3, node_id := _Node_id3}}
                            ] = [dict:fetch(Node, Pos) || Node <- Nodes],
                            {X_p, Y_p} = trilateration({V1, Pos_x1, Pos_y1}, {V2, Pos_x2, Pos_y2}, {V3, Pos_x3, Pos_y3}),
                            Result = io_lib:format("x, ~.2f, y, ~.2f", [X_p, Y_p]),
                            {ok, Result};
                        [{_, _}, {_, _}, {_, _}, {_, _}] ->
                            Neighbors = lists:filter(fun(N) -> neighbors(Node_id, dict:fetch(N, Pos)) end, Nodes),
                            [{_Seq1, V1}, {_Seq2, V2}, {_Seq3, V3}] = [dict:fetch(Node, Sonar) || Node <- Neighbors],
                            [
                                {_, #{x := Pos_x1, y := Pos_y1, node_id := _Node_id1}},
                                {_, #{x := Pos_x2, y := Pos_y2, node_id := _Node_id2}},
                                {_, #{x := Pos_x3, y := Pos_y3, node_id := _Node_id3}}
                            ] = [dict:fetch(Node, Pos) || Node <- Neighbors],
                            {X_p, Y_p} = trilateration({V1, Pos_x1, Pos_y1}, {V2, Pos_x2, Pos_y2}, {V3, Pos_x3, Pos_y3}),
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

neighbors(Node_id, {_, #{node_id := Id}}) ->
    if
        Id =:= Node_id -> true;
        Id =:= (Node_id + 4 + 1) rem 4 -> true;
        Id =:= (Node_id + 4 - 1) rem 4 -> true;
        true -> false
    end.