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

-export([launch_hera/1]).
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

launch_hera(Separation) ->
    Measurements = [{sonar, #{func => fun(Inch_to_cm) -> sonar_measurement(Inch_to_cm) end, args => [2.54], frequency => 100}}],
    Calculations = [{position, #{func => fun(Sep) -> calc_position(Sep) end, args => [Separation], frequency => 100}}],
    hera:launch_app(Measurements, Calculations).

%%%===================================================================
%%% Internal functions
%%%===================================================================

sonar_measurement(Inch_to_cm) ->
    case pmod_maxsonar:get() of
        undefined -> {error, "pmod_maxsonar not set up correctly"};
        Value -> {ok, Value*Inch_to_cm}
    end.

calc_position(Separation) ->
    Data = hera:get_data(sonar),
    Values = dict:to_list(Data),
    Length = dict:size(Data),

    if  % assign ready2 to true if set contains 2 measures
        Length =:= 2 ->
            %[{R1, _},{R2, _}] = sets:to_list(Values), % [{measure, name}, ...]
            [{_Seqnum1, R1}, {_Seqnum2, R2}] = [dict:fetch(Node, Values) || Node <- dict:fetch_keys(Values)],
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
    end.