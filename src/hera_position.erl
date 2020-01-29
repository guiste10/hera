-module(hera_position).
-behaviour(gen_server).
-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
handle_info/2, code_change/3, terminate/2]).
 
start_link(Delay) ->
    io:format("position: startlink ~n"),
    gen_server:start_link(?MODULE, Delay, []).
 
stop(Pid) ->
    gen_server:call(Pid, stop).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Delay) ->
    Id = {<<"measurements">>,state_orset},
    %io:format("position: wait ~n"),
    %lasp:read(Id, {cardinality, 2}), % wait until set contains 2 measures
    Separation = 400,
    Iter = 0,
    io:format("position: init ~n"),
    {ok, {Delay, Id, Separation, Iter}, Delay}. % {ok, state, timeout}

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.
        
handle_cast(_Msg, State) ->
    {noreply, State}.
        
handle_info(timeout, {Delay, Id, Separation, Iter}) ->
    io:format("position: worker handling info ~n"),
    {ok, Values} = lasp:query(Id), 
    Length = sets:size(Values),
    if  % assign ready2 to true if set contains 2 measures
        Length =:= 2 ->
            [{R1, _},{R2, _}] = sets:to_list(Values), % [{measure, name}, ...]
            R1Sq = math : pow ( R1 , 2) ,
            R2Sq = math : pow ( R2 , 2) ,
            S2 = 2 * Separation ,
            SSq = math : pow ( Separation , 2) ,
            X = ( R1Sq - R2Sq + SSq ) / S2 ,
            Helper = R1Sq - math : pow (X , 2),
            if
                Helper < 0 ->
                    io:format("position: not definable: square root of neg number ~n");
                true ->
                    Y1 = math : sqrt ( Helper ) ,
                    Y2 = - Y1 ,
                    io:format("position: (~p, ~p) or (~p, ~p) ~n", [X, Y1, X, Y2])
            end;
        true ->
            io:format("position: not definable: not 2 available measures ~n")
    end,
    {noreply, {Delay, Id, Separation, Iter+1}, Delay}.
%% We cannot use handle_info below: if that ever happens,
%% we cancel the timeouts (Delay) and basically zombify
%% the entire process. It's better to crash in this case.
%% handle_info(_Msg, State) ->
%%    {noreply, State}.
        
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
        
terminate(_Reason, _State) -> ok.