-module(hera_position).
-behaviour(gen_server).
-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
handle_info/2, code_change/3, terminate/2]).
 
start_link(Delay) ->
    gen_server:start_link(?MODULE, Delay, []).
 
stop(Pid) ->
    gen_server:call(Pid, stop).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Delay) ->
    Id = {<<"measurements">>,state_orset},
    lasp:read(Id, {cardinality, 2}), % wait until set contains 2 measures
    Separation = 4,
    Iter = 0,
    {ok, {Delay, Id, Separation, Iter}, Delay}. % {ok, state, timeout}

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.
        
handle_cast(_Msg, State) ->
    {noreply, State}.
        
handle_info(timeout, {Delay, Id, Separation, Iter}) ->
    {ok, Value} = lasp:query(Id), 
    [{R1, _},{R2, _}] = sets:to_list(Value), % [{measure, name}, ...]
    R1Sq = math : pow ( R1 , 2) ,
    R2Sq = math : pow ( R2 , 2) ,
    S2 = 2 * Separation ,
    SSq = math : pow ( Separation , 2) ,
    X = ( R1Sq - R2Sq + SSq ) / S2 ,
    Y1 = math : sqrt ( R1Sq - math : pow (X , 2) ) ,
    Y2 = - Y1 ,
    io:display("position: (~p, ~p) or (~p, ~p) ~n", [X, Y1, X, Y2]),
    {noreply, {Delay, Id, Separation, Iter+1}, Delay}.
%% We cannot use handle_info below: if that ever happens,
%% we cancel the timeouts (Delay) and basically zombify
%% the entire process. It's better to crash in this case.
%% handle_info(_Msg, State) ->
%%    {noreply, State}.
        
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
        
terminate(_Reason, _State) -> ok.