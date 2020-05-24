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

-export([start_link/6, stop/1]).

-export([pause_measurement/1, restart_measurement/1, restart_measurement/3, restart_measurement/6]).

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
    max_iterations :: integer() | infinity
}).
-type state() :: #state{}.
 
%%%===================================================================
%%% API
%%%===================================================================

%% @private
%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Name :: atom(), Measurement_func :: function(), Func_args :: list(any()), Delay :: integer(), Filtering :: boolean(), Max_iterations :: integer() | infinity) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name, Measurement_func, Args, Delay, Filtering, Max_iterations) ->
    gen_server:start_link(?MODULE, {Name, Measurement_func, Args, Delay, Filtering, Max_iterations}, []).

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
%% @param Max_iterations The number of iterations to be done
%% @param Filtering Boolean that indicates if a filtering must be done to the data output by the function
%%
%% @spec restart_measurement(Name :: atom(), Func :: fun((...) -> {ok, term()} | {error, term()}), Args :: list(any()), Frequency :: integer(), Max_iterations :: integer(), Filtering :: boolean()) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec restart_measurement(Name :: atom(), Func :: fun((...) -> {ok, term()} | {error, term()}), Args :: list(any()), Frequency :: integer(), Max_iterations :: integer(), Filtering :: boolean()) -> ok.
restart_measurement(Name, Func, Args, Frequency, Max_iterations, Filtering) ->
    gen_server:cast(Name, {restart, {Func, Args, Frequency, Max_iterations, Filtering}}).
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
%% @param Max_iterations The number of iterations to be done
%%
%% @spec restart_measurement(Name :: atom(), Frequency :: integer(), Max_iterations :: integer() | infinity) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec restart_measurement(Name :: atom(), Frequency :: integer(), Max_iterations :: integer() | infinity) -> ok.
restart_measurement(Name, Frequency, Max_iterations) ->
    gen_server:cast(Name, {restart, {Frequency, Max_iterations}}).

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

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
%% @doc Initializes the server
-spec(init({Name :: atom(), Measurement_func :: function(), Args :: list(any()), Delay :: integer(), Max_iterations :: integer() | infinity}) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init({Name, Measurement_func, Args, Delay, Filtering, Max_iterations}) ->
    {ok, #state{name = Name, measurement_func = Measurement_func, func_args = Args, delay = Delay, iter = 0, default_Measure = {-1.0, -1}, filtering = Filtering, max_iterations = Max_iterations}, Delay}. % {ok, state, timeout}

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
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: state()}).
handle_cast(restart, State = #state{delay = Delay}) ->
    {noreply, State, Delay};
handle_cast({restart, {Frequency, Max_iterations}}, State) ->
    {noreply, State#state{iter = 0, max_iterations = Max_iterations, delay = Frequency, warm_up = true, default_Measure = {-1.0, -1}}, Frequency};
handle_cast({restart, {Func, Args, Delay, Max_iter, Filtering}}, State) ->
    {noreply, State#state{iter = 0, measurement_func = Func, func_args = Args, max_iterations = Max_iter, delay = Delay, filtering = Filtering, warm_up = true, default_Measure = {-1.0, -1}}, Delay};
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
handle_info(timeout, State = #state{name = Name, measurement_func = Func, func_args = Args, iter = Iter, delay = Delay, filtering = Do_filter, warm_up = Warm_up, default_Measure = Default_m, max_iterations = Max_iterations}) ->
    Default_Measure = case Warm_up of
                          true -> perform_sonar_warmup(Func, Args, Name);
                          false -> Default_m
                      end,
    Measure_timestamp = hera:get_timestamp(),
    case erlang:apply(Func, Args) of
        {error, Reason} -> logger:error(Reason);
        {ok, Measure} ->
            if
                Do_filter == true ->
                    hera_filter:filter({Measure, Measure_timestamp}, Iter, Default_Measure, Name);
                true ->
                    hera:store_data(Name, node(), Iter, Measure),
                    hera:send(measure, Name, node(), Iter, {Measure, Measure_timestamp})
            end
    end,
    case Max_iterations-1 of
        Iter -> {noreply, State#state{iter = 0}, hibernate};
        _ -> {noreply, State#state{iter = Iter+1 rem ?MAX_SEQNUM, default_Measure = Default_Measure, warm_up = false}, Delay}
    end;

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
-spec perform_sonar_warmup(Measure_func :: function(), Args :: list(any()), Name :: atom()) ->
    Default_Measure :: {float(), integer()}.
perform_sonar_warmup(Measure_func, Args, Name) ->
    perform_sonar_warmup_aux(0, 100, 50, Measure_func, Args, Name). % hardcodé, récup 100ième mesure

% todo: make maxiter/2 unused measures, then return median of next maxiter/2 measures? or osef just send last measure?
-spec perform_sonar_warmup_aux(Iter :: integer(), Max_iter :: integer(), Delay :: integer(), Measure_func :: function(), Args :: list(any()), Name :: atom()) ->
    Default_Measure :: {float(), integer()}.
perform_sonar_warmup_aux(Iter, Max_iter, Delay, Measure_func, Args, Name) -> % todo, selec mediane de toutes les mesures
    if
        Iter < Max_iter-1 ->
            timer:sleep(Delay),
            perform_sonar_warmup_aux(Iter+1, Max_iter, Delay, Measure_func, Args, Name);
        Iter == Max_iter-1 ->
            {ok, Measure} = erlang:apply(Measure_func, Args),
            Measure_timestamp = hera:get_timestamp(),
            hera:send(measure, Name, node(), -1, {Measure, Measure_timestamp}),
            {Measure, Measure_timestamp}
    end.