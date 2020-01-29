-module(hera_measure).
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
    Iter = 0,
    Id = {<<"measurements">>, state_orset},
    {ok, {Delay, Id, Iter}, Delay}. % {ok, state, timeout}

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.
        
handle_cast(_Msg, State) ->
    {noreply, State}.
        
handle_info(timeout, {Delay, Id, Iter}) ->
    Measure = pmod_maxsonar:get() * 2.54,
    % Measure = hera:fake_sonar_get(),
    io:format("measure: (~p) ~n", [Measure]),
    Name = node(),
    {ok, Value} = lasp:query(Id),
    io:format("set: (~p) ~n", [Value]),
    % S1, the set containing only values for Name
    S1 = sets:filter(fun(_Elem = {_Val, N}) -> N == Name end, Value),
    Length = sets:size(S1),
    if 
        Length > 0 ->
            [{R1, Name}] = sets:to_list(S1),
            lasp:update(Id, {rmv, {R1, Name}}, self()),
            lasp:update(Id, {add, {Measure, Name}}, self());
        true ->
            lasp:update(Id, {add, {Measure, Name}}, self())
    end,
    {noreply, {Delay, Id, Iter+1}, Delay}.
%% We cannot use handle_info below: if that ever happens,
%% we cancel the timeouts (Delay) and basically zombify
%% the entire process. It's better to crash in this case.
%% handle_info(_Msg, State) ->
%%    {noreply, State}.
        
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
        
terminate(_Reason, _State) -> ok.