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

launch_hera(PosX, PosY, NodeId) ->
    Measurements = [
        {sonar, #{func => fun(InchToCm) -> sonar_measurement(InchToCm) end, args => [2.54], frequency => 100, 
        filtering => true, upperBound => 0.14,
         max_iterations => 100}},
        {pos, #{func => fun() -> {ok, #{x => PosX, y => PosY, node_id => NodeId}} end, args => [], frequency => 50, filtering => false, upperBound => 0.28, max_iterations => 3}}
    ],
    %Calculations = [{position, #{func => fun(Id) -> calc_position(Id) end, args => [NodeId], frequency => 100, max_iterations => 150}}],
    Calculations = [], % no calculation
    hera:launch_app(Measurements, Calculations).

launch_hera(PosX, PosY, NodeId, Frequency, MaxIteration) ->
    Measurements = [
        {sonar, #{func => fun(InchToCm) -> sonar_measurement(InchToCm) end, args => [2.54], frequency => Frequency, 
        filtering => true, upperBound => 0.14,
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
    %case hera:get_data(sonar) of   % works
    case hera:get_recent_data(sonar) of
        {error, Reason} ->
            logger:error(Reason),
            error;
        {ok, Sonar} ->
            case hera:get_data(pos) of
                {error, Reason} ->
                    logger:error(Reason),
                    error;
                {ok, Pos} ->
                    Nodes = lists:filter(fun(N) -> dict:is_key(N, Pos) end, dict:fetch_keys(Sonar)), % fetch all nodes from the sonar measurements of who we received the position
                    Values = [dict:fetch(Node, Sonar) || Node <- Nodes],
                    case Values of
                        [{_Seq1, R1, _T1}, {_Seq2, R2, _T2}] ->
                            [
                                {_, #{x := PosX1, y := PosY1, node_id := _NodeId1},_},
                                {_, #{x := PosX2, y := PosY2, node_id := _NodeId2},_}
                            ] = [dict:fetch(Node, Pos) || Node <- Nodes],
                            Res = filtered_trilateration({R1, PosX1, PosY1}, {R2, PosX2, PosY2}, MaxX, MaxY), % todo pass maxX and MaxY
                            case Res of
                                {none, exceedBounds} ->
                                    {error, "Position not definable: no position that doesn't exceed imposed bounds~n"};
                                {none, negativeRoot} ->
                                    {error, "Position not definable: square root of neg number~n"};
                                {X1, Y1} -> 
                                    Result = io_lib:format("x, ~.2f, y, ~.2f", [X1, Y1]),
                                    {ok, Result};
                                {{X1, Y1}, {X2, Y2}} -> 
                                    Result = io_lib:format("x1, ~.2f, y1, ~.2f or x2, ~.2f, y2, ~.2f", [X1, Y1, X2, Y2]),
                                    {ok, Result}
                            end;
                        [{_Seq1, V1, _T1}, {_Seq2, V2, _T2}, {_Seq3, V3, _T3}] ->
                            [
                                {_, #{x := PosX1, y := PosY1, node_id := _NodeId1},_},
                                {_, #{x := PosX2, y := PosY2, node_id := _NodeId2},_},
                                {_, #{x := PosX3, y := PosY3, node_id := _NodeId3},_}
                            ] = [dict:fetch(Node, Pos) || Node <- Nodes],
                            {X_p, Y_p} = trilateration({V1, PosX1, PosY1}, {V2, PosX2, PosY2}, {V3, PosX3, PosY3}),
                            Result = io_lib:format("x, ~.2f, y, ~.2f", [X_p, Y_p]),
                            {ok, Result};
                        [{_, _, _}, {_, _, _}, {_, _, _}, {_, _, _}] ->
                            Neighbors = lists:filter(fun(N) -> neighbors(NodeId, dict:fetch(N, Pos)) end, Nodes),
                            [{_Seq1, V1}, {_Seq2, V2}, {_Seq3, V3}] = [dict:fetch(Node, Sonar) || Node <- Neighbors],
                            [
                                {_, #{x := PosX1, y := PosY1, node_id := _NodeId1},_},
                                {_, #{x := PosX2, y := PosY2, node_id := _NodeId2},_},
                                {_, #{x := PosX3, y := PosY3, node_id := _NodeId3},_}
                            ] = [dict:fetch(Node, Pos) || Node <- Neighbors],
                            {X_p, Y_p} = trilateration({V1, PosX1, PosY1}, {V2, PosX2, PosY2}, {V3, PosX3, PosY3}),
                            Result = io_lib:format("x, ~.2f, y, ~.2f", [X_p, Y_p]),
                            {ok, Result};
                        %[{_, _, _}, {_, _, _}, {_, _, _}, {_, _, _}] ->
                        % todo: add trilateration multitarget 4 sonars and make it receive maxX and MaxY as argument from launchhera()
                        % todo: don't set limit on age of stored value so that always 4 measures available?
                        % todo: use trilateration method below with 4 arguments ABCD + log the 2 positions found
                        _ ->
                            {error, "Not the right number of measures available"}
                    end
            end
    end.

% used by trilateration when only 2 sonar measures are available, and also when tracking 2 targets using 4 sonar measures.
filtered_trilateration(Measure1, Measure2, MaxX, MaxY) -> 
    try trilateration(Measure1, Measure2) of
        {A, A} ->
            A; % don't return twice the same position
        {A, B} ->
            filter_2_positions(A, B, MaxX, MaxY)
    catch
        error:_ -> {none, negativeRoot} % no position possible
    end.

% filters the target positions found by the trilateration that uses 2 sonar measures.
% used by trilateration when only 2 sonar measures are available, and also when tracking 2 targets using 4 sonar measures.
% when tracking 2 targets with 4 sonars, chosen sonars will be on a rectangle, and only contiguous pairs of sonars
% will perform trilateration so that the ambiguity can be avoided by only considering valid the target positions inside that rectangle
filter_2_positions({X1, Y1}=PosA, {X2, Y2}=PosB, MaxX, MaxY) ->
    if
        (0 =< X1 andalso X1 =< MaxX andalso 0 =< Y1 andalso Y1 =< MaxY) andalso 
        (0 =< X2 andalso X2 =< MaxX andalso 0 =< Y2 andalso Y2 =< MaxY) ->
            [PosA, PosB];
        0 =< X1 andalso X1 =< MaxX andalso 0 =< Y1 andalso Y1 =< MaxY ->
            PosA;
        0 =< X2 andalso X2 =< MaxX andalso 0 =< Y2 andalso Y2 =< MaxY ->
            PosB;
        true ->
            {none, exceedBounds} % none of the 2 positions respect the maxX and maxY limits
    end.

trilateration({R1, X1, Y}, {R2, X2, Y}) -> % sonars are at the same height
    U = X2-X1,
    Helper1 = math:pow(R1, 2) - math:pow(R2, 2) + math:pow(U, 2),
    TargetX = Helper1/(2*U),
    TargetY = math:sqrt(math:pow(R1, 2) - (math:pow(Helper1, 2)/(4*math:pow(U, 2)))),
    TargetY2 = -TargetY,
    {{TargetX+X1,TargetY+Y}, {TargetX+X1,TargetY2+Y}};
trilateration({R, X1, Y1}, {S, X2, Y2}) ->
    {U,V} = {X2-X1,Y2-Y1},
    [UPow2, VPow2, RPow2, SPow2] = [math:pow(X,2) || X <- [U, V, R, S]],
    Helper1 = RPow2*U,
    Helper2 = RPow2*VPow2,
    HelperUV = UPow2 + VPow2,
    HelperRoot = math:sqrt(-VPow2*  ( math:pow(R, 4)-2*RPow2*(SPow2 + HelperUV) + math:pow(-SPow2 + HelperUV, 2)  )),
    Helper4 = -SPow2*U + math:pow(U, 3) + U*VPow2,
    Helper5 = -SPow2*VPow2 + UPow2*VPow2 + math:pow(V, 4),
    Helper6 = 2*HelperUV,
    TargetX1 = (Helper1 - HelperRoot + Helper4)/Helper6,
    TargetY1 = (Helper2 + U*HelperRoot + Helper5)/(V*Helper6),
    TargetX2 = (Helper1 + HelperRoot + Helper4)/Helper6,
    TargetY2 = (Helper2 - U*HelperRoot + Helper5)/(V*Helper6),
    {{TargetX1+X1,TargetY1+Y1}, {TargetX2+X1,TargetY2+Y1}}.
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


  %test:trilateration({math:sqrt(8), 0, 0}, {math:sqrt(8), 0, 4},{math:sqrt(8), 10, 4}, {math:sqrt(8), 10, 0}, 10, 4). gives 8,2 and 2,2
  trilateration(A, B, C, D, MaxX, MaxY) ->
    PosL = get_target_positions([A, B, C, D], A, [], MaxX, MaxY),
    case PosL of
        [] -> 
            {none, exceedBounds};
        [Pos] -> % only one target position found
            Pos;
        [FirstPos|OtherPosL] -> % one or more target positions found
            get_target_positions(OtherPosL, FirstPos)
    end.

% returns 2 different positions, or one if the 2 targets are really close to one another
get_target_positions([], FirstPos) ->
    FirstPos;
get_target_positions([Pos|OtherPosL], FirstPos) ->
    SameTarget = same_target(Pos, FirstPos),
    if
        SameTarget == true ->
            get_target_positions(OtherPosL, FirstPos);
        true ->
            [FirstPos, Pos] % found 2 different targets
    end.



% targets are considered the same if the distance between them < 40
same_target({X1, Y1}, {X2, Y2}) ->
    DeltaX = X1-X2,
    DeltaY = Y1-Y2,
    math:sqrt(math:pow(DeltaX, 2) + math:pow(DeltaY, 2)) < 40.


% returns the positions of all targets detected using a pair of sonars each time, 4 sonars -> 4 contiguous pairs of sonars
get_target_positions([LastMeasure], FirstMeasure, PosL, MaxX, MaxY) ->
    Pos = filtered_trilateration(LastMeasure, FirstMeasure, MaxX, MaxY), % only 1 pos should be received, because sonars with these 2 measures are contiguous
    add_to_position_list(Pos, PosL);
get_target_positions([Measure|MeasureL], FirstMeasure, PosL, MaxX, MaxY) -> % measureL length > 1
    Pos = filtered_trilateration(Measure, hd(MeasureL), MaxX, MaxY),
    PosL2 = add_to_position_list(Pos, PosL),
get_target_positions(MeasureL, FirstMeasure, PosL2, MaxX, MaxY).
    

% add the position to the list of positions.
add_to_position_list(Pos, PosL) ->
    case Pos of
        {none,_Reason} ->
            PosL;
        _ ->
            [Pos|PosL]
    end.










neighbors(NodeId, {_, #{node_id := Id}}) ->
    if
        Id =:= NodeId -> true;
        Id =:= (NodeId + 4 + 1) rem 4 -> true;
        Id =:= (NodeId + 4 - 1) rem 4 -> true;
        true -> false
    end.

