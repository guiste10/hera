-module(single_sonar_test).
-behaviour(gen_server).
-export([start_link/3, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
handle_info/2, code_change/3, terminate/2]).

%%====================================================================
%% Macros
%%====================================================================

-define(SERVER, ?MODULE).

%%====================================================================
%% Records
%%====================================================================

-record(state, {
    iter :: integer(),
    max_iter :: integer(),
    delay :: integer(),
    file :: file:io_device(),
    filename :: file:name_all()
}).
-type state() :: #state{}.
 
%%%===================================================================
%%% API
%%%===================================================================

start_link(Delay, Max_iter, File_name) ->
    gen_server:start_link(?MODULE, {Delay, Max_iter, File_name}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init({Delay, Max_iter, File_name}) ->
    {ok, File} = file:open(File_name, [read, write]),
    Iter = 0,
    {ok, #state{iter = Iter, max_iter = Max_iter, delay = Delay, file = File, filename = File_name}, Delay}. % {ok, state, timeout}

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.
        
handle_cast(_Msg, State) ->
    {noreply, State}.
        
handle_info(timeout, State) ->
    Measure = pmod_maxsonar:get() * 2.54,
    Measure_str = io_lib:format("~.2f", [Measure]), % pour vrai sonar (float)
    %Measure = hera:fake_sonar_get(),
    %Measure_str = integer_to_list(Measure), % pour faux sonar (integer)

    io:format("measure: (~s) ~n", [Measure_str]), % print
    Row = Measure_str ++ "\n",
    file:pwrite(State#state.file, eof, [Row]),
    if
        State#state.iter < State#state.max_iter-1 ->
            {noreply, State#state{iter = State#state.iter+1}, State#state.delay};
            true ->
                file:close(State#state.file),
               {noreply, State#state{iter = State#state.iter+1}}
               end.
%% We cannot use handle_info below: if that ever happens,
%% we cancel the timeouts (Delay) and basically zombify
%% the entire process. It's better to crash in this case.
%% handle_info(_Msg, State) ->
%%    {noreply, State}.
        
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
        
terminate(_Reason, _State) -> ok.