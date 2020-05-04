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

-export([start_link/5, stop/1]).

-export([pause/0]).

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
    warm_up = true :: boolean()
}).
-type state() :: #state{}.
 
%%%===================================================================
%%% API
%%%===================================================================

%% @private
%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Name :: atom(), Measurement_func :: function(), Func_args :: list(any()), Delay :: integer(), Filtering :: boolean()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name, Measurement_func, Args, Delay, Filtering) ->
    gen_server:start_link(?MODULE, {Name, Measurement_func, Args, Delay, Filtering}, []).

%% @private
-spec(stop(Pid :: pid()) ->
    term()).
stop(Pid) ->
    gen_server:call(Pid, stop).

%%--------------------------------------------------------------------
%% @doc
%% Log the given measure into a file with the same name as the node name
%%
%% @param Name The name/type of the measure (e.g. temperature, sonar, humidity, ...)
%%
%% @spec pause(Name :: atom()) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec pause() -> ok.
pause() ->
    gen_server:cast(?SERVER, pause).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
%% @doc Initializes the server
-spec(init({Name :: atom(), Measurement_func :: function(), Args :: list(any()), Delay :: integer()}) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init({Name, Measurement_func, Args, Delay}) ->
    {ok, #state{name = Name, measurement_func = Measurement_func, func_args = Args, delay = Delay, iter = 0, default_Measure = {-1.0, -1}}, Delay}. % {ok, state, timeout}

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
handle_info(timeout, State = #state{name = Name, measurement_func = Func, func_args = Args, iter = Iter, delay = Delay, filtering = Do_filter, warm_up = Warm_up, default_Measure = Default_m}) ->
    %%TODO : allow to pause the storing and sending, then restart
    Default_Measure = case Warm_up of
                          true -> perform_sonar_warmup(Func, Args);
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
                    hera:send({measure, Name, {node(), Iter, Measure}})
            end
    end,
    {noreply, State#state{iter = Iter+1 rem ?MAX_SEQNUM, default_Measure = Default_Measure, warm_up = false}, Delay};

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
-spec perform_sonar_warmup(Measure_func :: function(), Args :: list(any())) ->
    Default_Measure :: {float(), integer()}.
perform_sonar_warmup(Measure_func, Args) ->
    perform_sonar_warmup_aux(0, 100, 50, Measure_func, Args). % hardcodé, récup 100ième mesure

% todo: make maxiter/2 unused measures, then return median of next maxiter/2 measures? or osef just send last measure?
-spec perform_sonar_warmup_aux(Iter :: integer(), Max_iter :: integer(), Delay :: integer(), Measure_func :: function(), Args :: list(any())) ->
    Default_Measure :: {float(), integer()}.
perform_sonar_warmup_aux(Iter, Max_iter, Delay, Measure_func, Args) -> % todo, selec mediane de toutes les mesures
    if
        Iter < Max_iter-1 ->
            timer:sleep(Delay),
            perform_sonar_warmup_aux(Iter+1, Max_iter, Delay, Measure_func, Args);
        Iter == Max_iter-1 ->
            Measure = erlang:apply(Measure_func, Args),
            Measure_timestamp = hera:get_timestamp(),
            {Measure, Measure_timestamp}
    end.