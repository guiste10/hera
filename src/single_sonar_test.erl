-module(single_sonar_test).
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
    {ok, File} = file:open("sonar_measures.txt", [write]),
    Iter = 0,
    {ok, {Iter, Delay, File}, Delay}. % {ok, state, timeout}

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.
        
handle_cast(_Msg, State) ->
    {noreply, State}.
        
handle_info(timeout, {Iter, Delay, File}) ->
    Measure = pmod_maxsonar:get() * 2.54,
    %Measure = hera:fake_sonar_get(),
    %io:format("measure: (~p) ~n", [Measure]), % print
    io:format(File, "~p~n", [Measure]),
    {noreply, {Iter+1, Delay, File}, Delay}.
%% We cannot use handle_info below: if that ever happens,
%% we cancel the timeouts (Delay) and basically zombify
%% the entire process. It's better to crash in this case.
%% handle_info(_Msg, State) ->
%%    {noreply, State}.
        
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
        
terminate(_Reason, _State) -> ok.