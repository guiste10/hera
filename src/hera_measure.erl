%%%-------------------------------------------------------------------
%%% @author Julien Bastin <julien.bastin@student.uclouvain.be>
%%% @author Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% Module called to perform some measurements with a certain frequency
%%% @end
%%%
%%%-------------------------------------------------------------------

-module(hera_measure).
-author("Julien Bastin <julien.bastin@student.uclouvain.be>, Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>").

-behaviour(gen_server).

-include("hera.hrl").

-export([start_link/1, stop/1]).

-export([pause_measurement/1, restart_measurement/1, restart_measurement/3, restart_measurement/5, perform_single_measurement/1]).

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
    name :: atom(),
    measurement_func :: function(),
    delay :: integer(),
    iter :: integer(),
    default_Measure :: {float(), integer()},
    filtering :: boolean(),
    warm_up = true :: boolean(),
    warm_up_state :: warm_up_state(),
    max_iterations :: integer() | infinity,
    upperBound :: float(),
    synchronization :: boolean()
}).
-type state() :: #state{}.

-record(warm_up_state, {
    iter :: integer(),
    max_iter :: integer(),
    delay :: integer(),
    measures :: list(float())
}).
-type warm_up_state() :: #warm_up_state{}.
%%%===================================================================
%%% API
%%%===================================================================

%% @private
%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Measurement :: unsync_measurement() | sync_measurement()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Measurement) ->
    gen_server:start_link(?MODULE, Measurement, []).

%% @private
-spec(stop(Pid :: pid()) ->
    term()).
stop(Pid) ->
    gen_server:call(Pid, stop).

%%--------------------------------------------------------------------
%% @doc
%% Restart workers that performs the measurements
%%
%% @param Name The name of the measurement
%% @param Func The measurement function to be executed
%% @param Args The arguments of the function
%% @param Frequency The frequency of the measurement
%% @param MaxIterations The number of iterations to be done
%% @param Filtering Boolean that indicates if a filtering must be done to the data output by the function
%%
%% @spec restart_measurement(Name :: atom(), Func :: fun((...) -> {ok, term()} | {error, term()}), Args :: list(any()), Frequency :: integer(), MaxIterations :: integer(), Filtering :: boolean()) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec restart_measurement(Name :: atom(), Func :: fun((...) -> {ok, term()} | {error, term()}), Frequency :: integer(), MaxIterations :: integer(), Filtering :: boolean()) -> ok.
restart_measurement(Name, Func, Frequency, MaxIterations, Filtering) ->
    gen_server:cast(Name, {restart, {Func, Frequency, MaxIterations, Filtering}}).
%%--------------------------------------------------------------------
%% @doc
%% Restart worker that performs the measurement <Name>
%%
%% @param Name The name of the measurement
%%
%% @spec restart_measurement(Name :: atom()) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec restart_measurement(Name :: atom()) -> ok.
restart_measurement(Name) ->
    gen_server:cast(Name, restart).

%%--------------------------------------------------------------------
%% @doc
%% Restart worker that performs the measurement <Name>
%%
%% @param Name The name of the measurement
%% @param Frequency The frequency of the measurement
%% @param MaxIterations The number of iterations to be done
%%
%% @spec restart_measurement(Name :: atom(), Frequency :: integer(), MaxIterations :: integer() | infinity) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec restart_measurement(Name :: atom(), Frequency :: integer(), MaxIterations :: integer() | infinity) -> ok.
restart_measurement(Name, Frequency, MaxIterations) ->
    gen_server:cast(Name, {restart, {Frequency, MaxIterations}}).

%%--------------------------------------------------------------------
%% @doc
%% Pause the worker that performs the measurement <Name>
%%
%% @param Name The name of the measurement
%%
%% @spec pause_measurement(Name :: atom()) -> ok.
%% @end
%%--------------------------------------------------------------------
pause_measurement(Name) ->
    gen_server:cast(Name, pause).

-spec perform_single_measurement(Name :: atom()) -> continue | stop.
perform_single_measurement(Name) ->
    gen_server:call(Name, single_measurement).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
%% @doc Initializes the server
-spec(init(sync_measurement() | unsync_measurement()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init({Name, #{func := MeasurementFunc, filtering := Filtering, upper_bound := UpperBound, max_iterations := MaxIterations, synchronization := true}})->
    hera_synchronization:make_measure_request(Name),
    {ok, form_state(Name, MeasurementFunc, undefined, undefined, Filtering, MaxIterations, UpperBound, true)};
init({Name, #{func := MeasurementFunc, frequency := Frequency, filtering := Filtering, upper_bound := UpperBound, max_iterations := MaxIterations, synchronization := false}})->
    {ok, form_state(Name, MeasurementFunc, Frequency, 100, Filtering, MaxIterations, UpperBound, false), Frequency}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: state()) ->
    {reply, Reply :: term(), NewState :: state()} |
    {reply, Reply :: term(), NewState :: state(), timeout() | hibernate} |
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: state()} |
    {stop, Reason :: term(), NewState :: state()}).
handle_call(get_default_measure, _From, State) ->
    {reply, State#state.default_Measure, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(single_measurement, _From, State) ->
    {NewState, Continuation} = perform_measurement(State),
    {reply, Continuation, NewState};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: state()}).
handle_cast(restart, State = #state{delay = Delay, synchronization = false}) ->
    {noreply, State, Delay};
handle_cast(restart, State = #state{name = Name, synchronization = true}) ->
    hera_synchronization:make_measure_request(Name),
    {noreply, State};
handle_cast({restart, {Frequency, MaxIterations}}, State = #state{synchronization = false}) ->
    {noreply, State#state{iter = 0, max_iterations = MaxIterations, delay = Frequency, warm_up = true, default_Measure = {-1.0, -1}}, Frequency};
handle_cast({restart, {Func, Delay, MaxIter, Filtering}}, State = #state{synchronization = false}) ->
    {noreply, State#state{iter = 0, measurement_func = Func, max_iterations = MaxIter, delay = Delay, filtering = Filtering, warm_up = true, default_Measure = {-1.0, -1}}, Delay};
handle_cast(pause, State) ->
    {noreply, State, hibernate};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: state()}).
handle_info(timeout, State = #state{synchronization = false}) ->
    perform_measurement(State);

%% We cannot use handle_info below: if that ever happens,
%% we cancel the timeouts (Delay) and basically zombify
%% the entire process. It's better to crash in this case.
handle_info(_Msg, State) ->
    {noreply, State}.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: state(),
    Extra :: term()) ->
    {ok, NewState :: state()} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: state()) -> term()).
terminate(_Reason, _State) -> ok.

%%====================================================================
%% Internal functions
%%====================================================================
%% @private
%% warmup phase
perform_measurement(State = #state{measurement_func = MeasureFunc
    , warm_up = true
    , synchronization = Sync
    , filtering = true
    , warm_up_state = #warm_up_state{iter = Iter, max_iter = MaxNumIter, measures = Measures, delay = Delay}})
    when Iter < MaxNumIter ->

    WarmUpState = State#state.warm_up_state,
    {ok, Measure} = MeasureFunc(),
    Measures2 = Measures ++ [Measure],
    case Sync of
        true -> {State#state{warm_up_state = WarmUpState#warm_up_state{iter = Iter+1, measures = Measures2}}, continue};
        false -> {noreply, State#state{warm_up_state = WarmUpState#warm_up_state{iter = Iter+1, measures = Measures2}}, Delay}
    end;

%% last iteration of warmup phase
perform_measurement(State = #state{name = Name
    , delay = Delay
    , warm_up = true
    , filtering = true
    , warm_up_state = #warm_up_state{max_iter = MaxNumIter, measures = Measures}
    , synchronization = Sync}) ->

    Measures2 = lists:sort(Measures),
    Median = lists:nth(MaxNumIter div 2 + 1, Measures2),
    MeasureTimestamp = hera:get_timestamp(),
    hera:send(measure, Name, node(), -1, {Median, MeasureTimestamp}),
    case Sync of
        true -> {State#state{warm_up = false, default_Measure = {Median, MeasureTimestamp}, warm_up_state = #warm_up_state{iter = 0, max_iter = 100, delay = undefined, measures = []}}, continue};
        false -> {noreply, State#state{warm_up = false, default_Measure = {Median, MeasureTimestamp}, warm_up_state = #warm_up_state{iter = 0, max_iter = 100, delay = undefined, measures = []}}, Delay}
    end;

%% normal phase without synchronization
perform_measurement(State = #state{name = Name
    , measurement_func = Func
    , iter = Iter
    , delay = Delay
    , filtering = DoFilter
    , default_Measure = DefaultM
    , max_iterations = MaxIterations
    , upperBound = UpperBound
    , synchronization = Sync})
    when Iter < MaxIterations ->

    normal_phase(Func, DoFilter, Iter, DefaultM, Name, UpperBound),
    case Sync of
        true -> {State#state{iter = Iter+1 rem ?MAX_SEQNUM}, continue};
        false -> {noreply, State#state{iter = Iter+1 rem ?MAX_SEQNUM}, Delay}
    end;

%% end of normal phase
perform_measurement(State = #state{synchronization = Sync})  ->

    logger:notice("[Measure] End of normal phase, State = ~p", [State]),

    case Sync of
        true -> {State#state{iter = 0, warm_up = true, warm_up_state = #warm_up_state{iter = 0, max_iter = 100, delay = undefined, measures = []}}, stop};
        false -> {noreply, State#state{iter = 0, warm_up = true, warm_up_state = #warm_up_state{iter = 0, max_iter = 100, delay = undefined, measures = []}}, hibernate}
    end.

%% @private
%% normal phase of the measurement
normal_phase(Func, DoFilter, Iter, DefaultM, Name, UpperBound) ->
    MeasureTimestamp = hera:get_timestamp(),
    case Func() of
        {error, Reason} -> logger:error(Reason);
        {ok, Measure} ->
            if
                DoFilter == true ->
                    hera_filter:filter({Measure, MeasureTimestamp}, Iter, DefaultM, Name, UpperBound);
                true ->
                    hera:store_data(Name, node(), Iter, Measure),
                    hera:send(measure, Name, node(), Iter, {Measure, MeasureTimestamp})
            end
    end.

%% @private
%% return a well-formed state
form_state(Name, MeasurementFunc, Frequency, WarmUpDelay, Filtering, MaxIterations, UpperBound, Synchronization) ->
    #state{name = Name
        , measurement_func = MeasurementFunc
        , delay = Frequency
        , iter = 0
        , default_Measure = {-1.0, -1}
        , warm_up_state = #warm_up_state{iter = 0, max_iter = 100, delay = WarmUpDelay, measures = []}
        , filtering = Filtering
        , max_iterations = MaxIterations
        , upperBound = UpperBound
        , synchronization = Synchronization}.