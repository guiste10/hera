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
-export([launch_hera_shell/1]).
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
        {sonar, #{func => fun(Inch_to_cm) -> sonar_measurement(Inch_to_cm) end, args => [2.54], frequency => 5000, filtering => true}},
        {pos, #{func => fun() -> {ok, #{x => Pos_x, y => Pos_y, node_id => Node_id}} end, args => [], frequency => 30000, filtering => false}}
    ],
    Calculations = [{position, #{func => fun(X, Y, Id) -> calc_position(Id) end, args => [Pos_x, Pos_y, Node_id], frequency => 5000}}],
    hera:launch_app(Measurements, Calculations).

launch_hera_shell(Separation) ->
    Measurements = [{sonar, #{func => fun() -> fake_sonar_m() end, args => [], frequency => 5000}}],
    Calculations = [{position, #{func => fun(Sep) -> calc_position(Sep) end, args => [Separation], frequency => 5000}}],
    hera:launch_app(Measurements, Calculations).

%%%===================================================================
%%% Internal functions
%%%===================================================================

fake_sonar_m() ->
    {ok, hera:fake_sonar_get()}.

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
                    Nodes = [N  || N <- [Node || Node <- dict:fetch_keys(Sonar)], dict:is_key(N, Pos)],
                    Length = length(Nodes),

                    if  % assign ready2 to true if set contains 2 measures
                        Length =:= 2 ->
                            [{_Seqnum1, R1}, {_Seqnum2, R2}] = [dict:fetch(Node, Sonar) || Node <- Nodes],
                            [#{x => Pos_x1, y => Pos_y1, node_id => _Node_id1}, #{x => Pos_x2, y => Pos_y2, node_id => _Node_id2}] = [dict:fetch(Node, Pos) || Node <- Nodes],
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
                                    Result = io_lib:format("position: (~.2f, ~.2f) or (~.2f, ~.2f)", [X, Y1, X, Y2]),
                                    {ok, Result}
                            end;
                        true ->
                            {error, "Not two mesurements available"}
                    end
            end
    end.