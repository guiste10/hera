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

-export([start_link/8, stop/1]).

-export([pause_measurement/1, restart_measurement/1, restart_measurement/3, restart_measurement/6]).
-export([trigger_measurement/1]).
-export([update_sync_phase/2]).

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
    func_args :: list(any()),
    delay :: integer(),
    iter :: integer(),
    default_Measure :: {float(), integer()},
    filtering :: boolean(),
    warm_up = true :: boolean(),
    max_iterations :: integer() | infinity,
    upperBound :: float(),
    synchronization :: boolean(),
    synchronization_phase :: boolean()
}).
-type state() :: #state{}.
 
%%%===================================================================
%%% API
%%%===================================================================

%% @private
%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Name :: atom(), MeasurementFunc :: function(), Func_args :: list(any()), Delay :: integer(), Filtering :: boolean(), MaxIterations :: integer() | infinity, UpperBound :: float(), Synchronization :: boolean()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name, MeasurementFunc, Args, Delay, Filtering, MaxIterations, UpperBound, Synchronization) ->
    gen_server:start_link(?MODULE, {Name, MeasurementFunc, Args, Delay, Filtering, MaxIterations, UpperBound, Synchronization}, []).

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
-spec restart_measurement(Name :: atom(), Func :: fun((...) -> {ok, term()} | {error, term()}), Args :: list(any()), Frequency :: integer(), MaxIterations :: integer(), Filtering :: boolean()) -> ok.
restart_measurement(Name, Func, Args, Frequency, MaxIterations, Filtering) ->
    gen_server:cast(hera:get_registered_name(Name, "measure"), {restart, {Func, Args, Frequency, MaxIterations, Filtering}}).
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
    gen_server:cast(hera:get_registered_name(Name, "measure"), restart).

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
    gen_server:cast(hera:get_registered_name(Name, "measure"), {restart, {Frequency, MaxIterations}}).

%%--------------------------------------------------------------------
%% @doc
%% Pause the worker that performs the measurement <Name>
%%
%% @param Name The name of the measurement
%%
%% @spec pause_measurement(Name :: atom()) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec pause_measurement(Name :: atom()) -> ok.
pause_measurement(Name) ->
    gen_server:cast(hera:get_registered_name(Name, "measure"), pause).

%%--------------------------------------------------------------------
%% @doc
%% Trigger the measurement of <Name>
%%
%% @param Name The name of the measurement
%%
%% @spec trigger_measurement(Name :: atom()) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec trigger_measurement(Name :: atom()) -> ok.
trigger_measurement(Name) ->
    gen_server:call(hera:get_registered_name(Name, "measure"), trigger).

update_sync_phase(Name, Phase) ->
    gen_server:call(hera:get_registered_name(Name, "measure"), {sync_phase, Phase}).
%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
%% @doc Initializes the server
-spec(init({Name :: atom(), MeasurementFunc :: function(), Args :: list(any()), Delay :: integer(), MaxIterations :: integer() | infinity, UpperBound :: float(), Synchronization :: boolean()}) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init({Name, MeasurementFunc, Args, Delay, Filtering, MaxIterations, UpperBound, Synchronization}) ->
    case Synchronization of
        true ->
            hera_synchronization:update_order(Name, queue:in_r(node(), hera_synchronization:get_order(Name))),
            hera_synchronization:update_measurement_phase(Name, true);
        false ->
            ok
    end,
    {ok, #state{name = Name
        , measurement_func = MeasurementFunc
        , func_args = Args
        , delay = Delay
        , iter = 0
        , default_Measure = {-1.0, -1}
        , filtering = Filtering
        , max_iterations = MaxIterations
        , upperBound = UpperBound
        , synchronization = Synchronization
        , synchronization_phase = false}, Delay}.

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
handle_call({sync_phase, Phase}, _From, State) ->
    {reply, ok, State#state{synchronization_phase = Phase}};
handle_call(trigger, _From, State) ->
    measure(State),
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: state()}).
handle_cast(restart, State = #state{delay = Delay, name = Name, synchronization = true}) ->
    hera_synchronization:update_order(Name, queue:in_r(node(), hera_synchronization:get_order(Name))),
    hera_synchronization:update_measurement_phase(Name, true),
    {noreply, State, Delay};
handle_cast(restart, State = #state{delay = Delay, synchronization = false}) ->
    {noreply, State, Delay};
handle_cast({restart, {Frequency, MaxIterations}}, State = #state{name = Name, synchronization = true}) ->
    hera_synchronization:update_order(Name, queue:in_r(node(), hera_synchronization:get_order(Name))),
    hera_synchronization:update_measurement_phase(Name, true),
    {noreply, State#state{iter = 0, max_iterations = MaxIterations, delay = Frequency, warm_up = true, default_Measure = {-1.0, -1}}, Frequency};
handle_cast({restart, {Frequency, MaxIterations}}, State = #state{synchronization = false}) ->
    {noreply, State#state{iter = 0, max_iterations = MaxIterations, delay = Frequency, warm_up = true, default_Measure = {-1.0, -1}}, Frequency};
handle_cast({restart, {Func, Args, Delay, MaxIter, Filtering}}, State = #state{name = Name, synchronization = true}) ->
    hera_synchronization:update_order(Name, queue:in_r(node(), hera_synchronization:get_order(Name))),
    hera_synchronization:update_measurement_phase(Name, true),
    {noreply, State#state{iter = 0, measurement_func = Func, func_args = Args, max_iterations = MaxIter, delay = Delay, filtering = Filtering, warm_up = true, default_Measure = {-1.0, -1}}, Delay};
handle_cast({restart, {Func, Args, Delay, MaxIter, Filtering}}, State = #state{synchronization = false}) ->
    {noreply, State#state{iter = 0, measurement_func = Func, func_args = Args, max_iterations = MaxIter, delay = Delay, filtering = Filtering, warm_up = true, default_Measure = {-1.0, -1}}, Delay};
handle_cast(pause, State = #state{name = Name, synchronization = true}) ->
    hera_synchronization:update_measurement_phase(Name, false),
    {noreply, State, hibernate};
handle_cast(pause, State = #state{synchronization = false}) ->
    {noreply, State, hibernate};
handle_cast(trigger, State) ->
    case measure(State) of
        {noreply, State, hibernate} -> {reply, ok, State, hibernate};
        {noreply, State} -> {reply, ok, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: state()}).
handle_info(timeout, State = #state{synchronization = false}) ->
    measure(State);
handle_info(timeout, State = #state{synchronization_phase = false, synchronization = true}) ->
    measure(State);

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
%% @doc only perform warmup when using real sonar
-spec perform_sonar_warmup(MeasureFunc :: function(), Args :: list(any()), Name :: atom()) ->
    DefaultMeasure :: {float(), integer()}.
perform_sonar_warmup(MeasureFunc, Args, Name) ->
    perform_sonar_warmup_aux(0, 200, 50, MeasureFunc, Args, Name, []). % hardcodé, récup 100ième mesure

% todo: make maxiter/2 unused measures, then return median of next maxiter/2 measures? or osef just send last measure?
-spec perform_sonar_warmup_aux(Iter :: integer(), MaxNumIter :: integer(), Delay :: integer(), MeasureFunc :: function(), Args :: list(any()), Name :: atom(), Measures :: list(float())) ->
    DefaultMeasure :: {float(), integer()}.
perform_sonar_warmup_aux(Iter, MaxNumIter, Delay, MeasureFunc, Args, Name, Measures) -> % todo, selec mediane de toutes les mesures
    if
        Iter < MaxNumIter ->
            {ok, Measure} = erlang:apply(MeasureFunc, Args),
            Measures2 = Measures ++ [Measure],
            timer:sleep(Delay),
            perform_sonar_warmup_aux(Iter+1, MaxNumIter, Delay, MeasureFunc, Args, Name, Measures2);
        Iter == MaxNumIter ->
            Measures2 = lists:sort(Measures),
            Median = lists:nth(MaxNumIter div 2 + 1, Measures2),
            MeasureTimestamp = hera:get_timestamp(),
            hera:send(measure, Name, node(), -1, {Median, MeasureTimestamp}),
            {Median, MeasureTimestamp}
    end.

measure(State = #state{name = Name
    , measurement_func = Func
    , func_args = Args
    , iter = Iter
    , delay = Delay
    , filtering = Do_filter
    , warm_up = WarmUp
    , default_Measure = DefaultM
    , max_iterations = MaxIterations
    , upperBound = UpperBound}) ->
    DefaultMeasure = case WarmUp of
                         true -> perform_sonar_warmup(Func, Args, Name);
                         false -> DefaultM
                     end,
    MeasureTimestamp = hera:get_timestamp(),
    case erlang:apply(Func, Args) of
        {error, Reason} -> logger:error(Reason);
        {ok, Measure} ->
            if
                Do_filter == true ->
                    hera_filter:filter({Measure, MeasureTimestamp}, Iter, DefaultMeasure, Name, UpperBound);
                true ->
                    hera:store_data(Name, node(), Iter, Measure),
                    hera_multicast:send({measure, Name, {node(), Iter, {Measure, MeasureTimestamp}}, hera_synchronization:get_order(Name)})
            end
    end,
    case MaxIterations-1 of
        Iter ->
            NewQueue = queue:filter(fun(Item) -> Item =/= node() end, hera_synchronization:get_order(Name)),
            hera_synchronization:update_order(Name, NewQueue),
            hera_synchronization:update_measurement_phase(Name, false),
            {noreply, State#state{iter = 0}, hibernate};
        _ -> {noreply, State#state{iter = Iter+1 rem ?MAX_SEQNUM, default_Measure = DefaultMeasure, warm_up = false}, Delay}
    end.