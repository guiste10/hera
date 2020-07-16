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

-export([launch_hera/5]).
-export([launch_hera/6]).
-export([launch_hera/7]).
-export([restart_calculation/2]).
-export([restart_measurement/1]).
-export([restart/2]).
-export([sonar_measurement/0]).
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

%%--------------------------------------------------------------------
%% @doc
%% Launch hera with synchronization to follow a person in a room.
%% In this case, a synchronization is performed between the measurement of the pmod_maxsonar in order to avoid cross-talking between them
%%
%% @param PosX The x coordinate of the board in the room
%% @param PosY The y coordinate of the board in the room
%% @param NodeId The id of the board. The first board must have NodeId = 0
%%
%%--------------------------------------------------------------------

-spec launch_hera(PosX :: integer(), PosY :: integer(), NodeId :: integer(), MaxX :: integer(), MaxY :: integer()) -> any().
launch_hera(PosX, PosY, NodeId, MaxX, MaxY) ->
    pmod_maxsonar:set_mode(single),
    Measurements = [
        hera:get_synchronized_measurement(sonar, fun() -> sonar_measurement() end, true, 0.14, infinity),
        hera:get_unsynchronized_measurement(pos, fun() -> {ok, #{x => PosX, y => PosY, node_id => NodeId}} end, false, 0.28, 3, 500)
    ],
    Calculations = [hera:get_calculation(position, fun() -> calc_position(NodeId, MaxX, MaxY) end, 50, infinity)],
    hera:launch_app(Measurements, Calculations).

launch_hera(PosX, PosY, NodeId, MaxIteration, MaxX, MaxY) ->
    pmod_maxsonar:set_mode(single),
    Measurements = [
        hera:get_synchronized_measurement(sonar, fun() -> sonar_measurement() end, true, 990.14, MaxIteration),
        hera:get_unsynchronized_measurement(pos, fun() -> {ok, #{x => PosX, y => PosY, node_id => NodeId}} end, false, 0.28, 3, 500)
    ],
    Calculations = [hera:get_calculation(position, fun() -> calc_position(NodeId, MaxX, MaxY) end, 50, MaxIteration)],
    hera:launch_app(Measurements, Calculations).

launch_hera(PosX, PosY, NodeId, Frequency, MaxIteration, MaxX, MaxY) ->
    Measurements = [
        hera:get_unsynchronized_measurement(sonar, fun() -> sonar_measurement() end, true, 0.14, MaxIteration, Frequency),
        hera:get_unsynchronized_measurement(pos, fun() -> {ok, #{x => PosX, y => PosY, node_id => NodeId}} end, false, 0.28, 3, 500)
    ],
    Calculations = [hera:get_calculation(position, fun() -> calc_position(NodeId, MaxX, MaxY) end, 50, MaxIteration)],
    %Calculations = [], % no calculation
    hera:launch_app(Measurements, Calculations).

restart(Frequency, MaxIterations) ->
    restart_measurement(MaxIterations),
    restart_calculation(Frequency, MaxIterations).

restart_calculation(Frequency, MaxIterations) ->
    hera:restart_calculation(position, Frequency, MaxIterations).

restart_measurement(MaxIterations) ->
    hera:restart_measurement(pos, false),
    hera:restart_sync_measurement(sonar, MaxIterations, false).

%%%===================================================================
%%% Internal functions
%%%===================================================================

sonar_measurement() ->
    case pmod_maxsonar:get() of
        undefined -> {error, "pmod_maxsonar not set up correctly"};
        Value ->
            {ok, Value*2.54}
    end.


calc_position(_NodeId, MaxX, MaxY) -> % todo remove nodeId if not using neighbours
    case hera_sensors_data:get_data(sonar) of
    %case hera_sensors_data:get_recent_data(sonar) of
        {error, Reason} ->
            logger:error(Reason),
            error;
        {ok, Sonar} ->
            case hera_sensors_data:get_data(pos) of
                {error, Reason} ->
                    logger:error(Reason),
                    error;
                {ok, Pos} ->
                    Nodes = lists:filter(fun(N) -> dict:is_key(N, Pos) end, dict:fetch_keys(Sonar)), % fetch all nodes from the recent sonar measurements of who we received the position
                    Values = [dict:fetch(Node, Sonar) || Node <- Nodes], 
                    % as many values as nodes of who we received the position
                    % values and positions are ordered according to nodes list
                    case Values of
                        [{_Seq1, R1, _T1}, {_Seq2, R2, _T2}] ->
                            [
                                {_, #{x := PosX1, y := PosY1, node_id := _NodeId1},_},
                                {_, #{x := PosX2, y := PosY2, node_id := _NodeId2},_}
                            ] = [dict:fetch(Node, Pos) || Node <- Nodes],
                            Res = filtered_trilateration({R1, PosX1, PosY1}, {R2, PosX2, PosY2}, MaxX, MaxY),
                            case Res of
                                {none, exceedBounds} ->
                                    {error, "Position not definable: no position that doesn't exceed imposed bounds~n"};
                                {none, negativeRoot} ->
                                    {error, "Position not definable: square root of neg number~n"};
                                {X1, Y1} -> 
                                    Result = io_lib:format("x, ~.2f, y, ~.2f", [X1, Y1]),
                                    {ok, Result};
                                [{X1, Y1}, {X2, Y2}] ->  % 2 possible positions
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
                        %[{_, _, _}, {_, _, _}, {_, _, _}, {_, _, _}] ->  \n ioformat? 2 targets?
                            %Neighbors = lists:filter(fun(N) -> neighbors(NodeId, dict:fetch(N, Pos)) end, Nodes),
                            %[{_Seq1, V1}, {_Seq2, V2}, {_Seq3, V3}] = [dict:fetch(Node, Sonar) || Node <- Neighbors],
                            %[
                            %    {_, #{x := PosX1, y := PosY1, node_id := _NodeId1},_},
                            %    {_, #{x := PosX2, y := PosY2, node_id := _NodeId2},_},
                            %    {_, #{x := PosX3, y := PosY3, node_id := _NodeId3},_}
                            %] = [dict:fetch(Node, Pos) || Node <- Neighbors],
                            %{X_p, Y_p} = trilateration({V1, PosX1, PosY1}, {V2, PosX2, PosY2}, {V3, PosX3, PosY3}),
                            %Result = io_lib:format("x, ~.2f, y, ~.2f", [X_p, Y_p]),
                            %{ok, Result};
                        [{_Seq1, V1, _T1}, {_Seq2, V2, _T2}, {_Seq3, V3, _T3}, {_Seq4, V4, _T4}] ->
                            [
                                {_, #{x := PosX1, y := PosY1, node_id := NodeId1},_},
                                {_, #{x := PosX2, y := PosY2, node_id := NodeId2},_},
                                {_, #{x := PosX3, y := PosY3, node_id := NodeId3},_},
                                {_, #{x := PosX4, y := PosY4, node_id := NodeId4},_}
                            ] = [dict:fetch(Node, Pos) || Node <- Nodes], % fetch pos of each node
                            Measures = [{NodeId1, {V1, PosX1, PosY1}}, {NodeId2, {V2, PosX2, PosY2}}, {NodeId3, {V3, PosX3, PosY3}}, {NodeId4, {V4, PosX4, PosY4}}],
                            ContiguousMeasuresId = lists:sort(Measures), % abcd are ordered according to nodeId to ensure they represent circular traversal of sonars
                            ContiguousMeasures = [element(2, E) || E <- ContiguousMeasuresId], % remove nodeid part and only keep the {V, PosX, PosY} tuple
                            Res = trilaterations_2_objects(ContiguousMeasures, MaxX, MaxY),
                            case Res of
                                {none, exceedBounds} ->
                                    {error, "No position found that doesn't violate MaxX and MaxY limits"};
                                {X1, Y1} ->
                                    Result = io_lib:format("x, ~.2f, y, ~.2f", [X1, Y1]),
                                    {ok, Result};
                                [{X1, Y1}, {X2, Y2}] -> % 2 different targets were detected
                                    Result = io_lib:format("x1, ~.2f, y1, ~.2f and x2, ~.2f, y2, ~.2f", [X1, Y1, X2, Y2]),
                                    {ok, Result}
                            end;
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

%test:trilateration({math:sqrt(8), 0, 0}, {math:sqrt(8), 0, 4},{math:sqrt(8), 10, 4}, {math:sqrt(8), 10, 0}, 10, 4). gives 8,2 and 2,2 when max dist < 4
trilaterations_2_objects(Measures, MaxX, MaxY) ->
    PosL = get_contiguous_pairs_positions(Measures, hd(Measures), [], MaxX, MaxY),
    case PosL of
        [] -> 
            {none, exceedBounds};
        [Pos] -> % only one target position found
            Pos;
        [FirstPos|OtherPosL] -> % one or more target positions found
            get_target_positions(OtherPosL, FirstPos)
    end.


% returns the positions of all targets detected using a pair of sonars each time, 4 sonars -> 4 contiguous pairs of sonars
get_contiguous_pairs_positions([LastMeasure], FirstMeasure, PosL, MaxX, MaxY) ->
    Pos = filtered_trilateration(LastMeasure, FirstMeasure, MaxX, MaxY), % only 1 pos should be received, because sonars with these 2 measures are contiguous
    add_to_position_list(Pos, PosL);
get_contiguous_pairs_positions([Measure|MeasureL], FirstMeasure, PosL, MaxX, MaxY) -> % measureL length > 1
    Pos = filtered_trilateration(Measure, hd(MeasureL), MaxX, MaxY),
    PosL2 = add_to_position_list(Pos, PosL),
    get_contiguous_pairs_positions(MeasureL, FirstMeasure, PosL2, MaxX, MaxY).
    
% add the position to the list of positions.
add_to_position_list(Pos, PosL) ->
    case Pos of
        {none,_Reason} ->
            PosL;
        _ ->
            [Pos|PosL]
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






neighbors(NodeId, {_, #{node_id := Id}}) ->
    if
        Id =:= NodeId -> true;
        Id =:= (NodeId + 4 + 1) rem 4 -> true;
        Id =:= (NodeId + 4 - 1) rem 4 -> true;
        true -> false
    end.

