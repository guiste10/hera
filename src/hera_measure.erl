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

-export([restart_unsync_measurement/4, restart_unsync_measurement/6]).
-export([restart_sync_measurement/3, restart_sync_measurement/5]).
-export([pause_measurement/1, restart_measurement/2, perform_single_measurement/1]).

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
    filtering :: function() | undefined,
    warm_up :: boolean(),
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
start_link(Measurement = {Name, _Map}) ->
    gen_server:start_link({local, Name}, ?MODULE, Measurement, []).

%% @private
-spec(stop(Pid :: pid()) ->
    term()).
stop(Pid) ->
    gen_server:call(Pid, stop).

%%--------------------------------------------------------------------
%% @doc
%% Restart worker that performs the unsynchronized measurement Name from zero with new parameters
%%
%% @param Name The name of the measurement
%% @param Func The measurement function to be executed
%% @param Frequency The frequency of the measurement
%% @param MaxIterations The number of iterations to be done
%% @param Filtering Filter function or the atom undefined
%% @param WarmUpPhase Boolean that indicates if a warm-up phase must be performed
%%
%% @spec restart_unsync_measurement(
%%            Name :: atom(),
%%            Func :: fun((...) -> {ok, term()} | {error, term()}),
%%            Frequency :: integer(),
%%            MaxIterations :: integer(),
%%            Filtering :: fun((any(), any(), integer(), list(any())) -> boolean() | any()) | undefined,
%%            WarmUpPhase :: boolean()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec restart_unsync_measurement(Name :: atom(), Func ::fun((...) -> {ok, term()} | {error, term()}), Frequency :: integer(), MaxIterations :: integer(),
    Filtering :: fun((any(), any(), integer(), list(any())) -> boolean() | any()) | undefined, WarmUpPhase :: boolean()) -> ok.
restart_unsync_measurement(Name, Func, Frequency, MaxIterations, Filtering, WarmUpPhase) ->
    gen_server:call(Name, {restart, {Func, Frequency, MaxIterations, Filtering, WarmUpPhase}}).

%%--------------------------------------------------------------------
%% @doc
%% Restart worker that performs the synchronized measurement Name from zero with new parameters
%%
%% @param Name The name of the measurement
%% @param Func The measurement function to be executed
%% @param MaxIterations The number of iterations to be done
%% @param Filtering Filter function or the atom undefined
%% @param WarmUpPhase Boolean that indicates if a warm-up phase must be performed
%%
%% @spec restart_sync_measurement(Name :: atom(), Func ::fun((...) -> {ok, term()} | {error, term()}), MaxIterations :: integer(),
%%    Filtering :: fun((any(), any(), integer(), list(any())) -> boolean() | any()) | undefined , WarmUpPhase :: boolean()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec restart_sync_measurement(Name :: atom(), Func ::fun((...) -> {ok, term()} | {error, term()}), MaxIterations :: integer(),
    Filtering :: fun((any(), any(), integer(), list(any())) -> boolean() | any()) | undefined , WarmUpPhase :: boolean()) -> ok.
restart_sync_measurement(Name, Func, MaxIterations, Filtering, WarmUpPhase) ->
    gen_server:call(Name, {restart, {Func, MaxIterations, Filtering, WarmUpPhase}}).

%%--------------------------------------------------------------------
%% @doc
%% Restart worker that performs the unsynchronized or synchronized measurement Name from zero if the previous calculation has terminated, or from the previous state if it is pause.
%%
%% @param Name The name of the measurement
%% @param WarmUpPhase Boolean that indicates if a warm-up phase must be performed
%%
%% @spec restart_measurement(Name :: atom(), WarmUpPhase :: boolean()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec restart_measurement(Name :: atom(), WarmUpPhase :: boolean()) -> ok.
restart_measurement(Name, WarmUpPhase) ->
    gen_server:call(Name, {restart, WarmUpPhase}).

%%--------------------------------------------------------------------
%% @doc
%% Restart worker that performs the synchronized measurement Name from zero if the previous calculation has terminated, or from the previous state if it is pause.
%%
%% @param Name The name of the measurement
%% @param MaxIterations The number of iterations to be done
%% @param WarmUpPhase Boolean that indicates if a warm-up phase must be performed
%%
%% @spec restart_sync_measurement(Name :: atom(), MaxIteration :: integer(), WarmUpPhase :: boolean()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec restart_sync_measurement(Name :: atom(), MaxIteration :: integer(), WarmUpPhase :: boolean()) -> ok.
restart_sync_measurement(Name, MaxIterations, WarmUpPhase) ->
    gen_server:call(Name, {restart, MaxIterations, WarmUpPhase}).

%%--------------------------------------------------------------------
%% @doc
%% Restart worker that performs the unsynchronized measurement Name from zero with new frequency and number of iterations
%%
%% @param Name The name of the measurement
%% @param Frequency The frequency of the measurement
%% @param MaxIterations The number of iterations to be done
%% @param WarmUpPhase Boolean that indicates if a warm-up phase must be performed
%%
%% @spec restart_unsync_measurement(Name :: atom(), Frequency :: integer(), MaxIterations :: integer(), WarmUpPhase :: boolean()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec restart_unsync_measurement(Name :: atom(), Frequency :: integer(), MaxIterations :: integer(), WarmUpPhase :: boolean()) -> ok.
restart_unsync_measurement(Name, Frequency, MaxIterations, WarmUpPhase) ->
    gen_server:call(Name, {restart, {Frequency, MaxIterations, WarmUpPhase}}).

%%--------------------------------------------------------------------
%% @doc
%% Pause the worker that performs the measurement Name
%%
%% @param Name The name of the measurement
%%
%% @spec pause_measurement(Name :: atom()) -> ok
%% @end
%%--------------------------------------------------------------------
pause_measurement(Name) ->
    gen_server:call(Name, pause).

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
init({Name, #{func := MeasurementFunc, filter := Filtering, upper_bound := UpperBound, max_iterations := MaxIterations, synchronization := true}})->
    hera_synchronization:make_measure_request(Name),
    {ok, form_state(Name, MeasurementFunc, undefined, undefined, Filtering, MaxIterations, UpperBound, true)};
init({Name, #{func := MeasurementFunc, frequency := Frequency, filter := Filtering, upper_bound := UpperBound, max_iterations := MaxIterations, synchronization := false}})->
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
handle_call({restart, {Frequency, MaxIterations, WarmUp}}, _From, State = #state{synchronization = false}) when is_integer(Frequency) and is_integer(MaxIterations) and is_boolean(WarmUp) ->
    {reply, ok, State#state{iter = 0, max_iterations = MaxIterations, delay = Frequency, warm_up = WarmUp}, Frequency};
handle_call({restart, MaxIterations, WarmUp}, _From, State = #state{name = Name, synchronization = true}) when is_integer(MaxIterations) and is_boolean(WarmUp)->
    hera_synchronization:make_measure_request(Name),
    {reply, ok, State#state{iter = 0, max_iterations = MaxIterations, warm_up = WarmUp}};
handle_call({restart, {Func, Delay, MaxIter, Filtering, WarmUp}}, _From, State = #state{synchronization = false})
    when is_function(Func) and is_integer(Delay) and is_integer(MaxIter) and (is_function(Filtering) or is_atom(Filtering)) and is_boolean(WarmUp) ->
    {reply, ok, State#state{iter = 0, measurement_func = Func, max_iterations = MaxIter, delay = Delay, filtering = Filtering, warm_up = WarmUp}, Delay};
handle_call({restart, {Func, MaxIter, Filtering, WarmUp}}, _From, State = #state{name = Name, synchronization = true})
    when is_function(Func) and is_integer(MaxIter) and (is_function(Filtering) or is_atom(Filtering)) and is_boolean(WarmUp)->
    hera_synchronization:make_measure_request(Name),
    {reply, ok, State#state{iter = 0, measurement_func = Func, max_iterations = MaxIter, filtering = Filtering, warm_up = WarmUp}};
handle_call({restart, WarmUp}, _From, State = #state{delay = Delay, synchronization = false}) when is_boolean(WarmUp) ->
    {reply, ok, State#state{warm_up = WarmUp}, Delay};
handle_call({restart, WarmUp}, _From, State = #state{name = Name, synchronization = true}) when is_boolean(WarmUp) ->
    hera_synchronization:make_measure_request(Name),
    {reply, ok, State#state{warm_up = WarmUp}};
handle_call(pause, _From, State) ->
    {reply, ok, State, hibernate};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: state()}).
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
    , filtering = Filter
    , warm_up_state = #warm_up_state{iter = Iter, max_iter = MaxNumIter, measures = Measures, delay = Delay}})
    when Iter < MaxNumIter, is_function(Filter) ->

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
    , filtering = Filter
    , warm_up_state = #warm_up_state{max_iter = MaxNumIter, measures = Measures}
    , synchronization = Sync}) when is_function(Filter) ->

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
    , default_Measure = {DefaultM, _Timestamp}
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
            case DoFilter of
                F when is_atom(F) ->
                    hera_sensors_data:store_data(Name, node(), Iter, Measure),
                    hera:send(measure, Name, node(), Iter, {Measure, MeasureTimestamp});
                Fu when is_function(Fu, 5) ->
                    hera_filter:filter(Name, {Measure, MeasureTimestamp}, Iter, UpperBound, [DefaultM]);
                Other -> logger:error("[Measurement] Wrong filter value : ~p", [Other])
            end
    end.

%% @private
%% return a well-formed state
form_state(Name, MeasurementFunc, Frequency, WarmUpDelay, Filtering, MaxIterations, UpperBound, Synchronization) ->
    #state{name = Name
        , measurement_func = MeasurementFunc
        , delay = Frequency
        , iter = 0
        , warm_up = true
        , default_Measure = {-1.0, -1}
        , warm_up_state = #warm_up_state{iter = 0, max_iter = 100, delay = WarmUpDelay, measures = []}
        , filtering = Filtering
        , max_iterations = MaxIterations
        , upperBound = UpperBound
        , synchronization = Synchronization}.