-module(hera_measure).
-behaviour(gen_server).
-export([start_link/0, stop/1, perform_measures/5]).
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
    default_Measure :: {float(), integer()}
}).
-type state() :: #state{}.
 
%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(stop(Pid :: pid()) ->
    term()).
stop(Pid) ->
    gen_server:call(Pid, stop).

%%--------------------------------------------------------------------
%% @doc
%% Perform measures
%%
%% @param Max_iter number of measures to perform
%% @param Delay frequency of measurements
%% @param Measure_func the function to perform the measure
%% @param Do_filter set to true for applying filter on measures
%% @param Measure_func set to true for performing a new warm up phase before the measurements
%%
%%--------------------------------------------------------------------
-spec perform_measures(Max_iter :: integer(), Delay :: integer(), Measure_func :: function(), Do_filter :: boolean(), Do_sonar_warmup :: boolean()) -> ok.
perform_measures(Max_iter, Delay, Measure_func, Do_filter, Do_sonar_warmup) ->
    if
        Do_sonar_warmup == true->
            gen_server:call(?SERVER, Do_sonar_warmup),
            io:format("warmup done ~n", []);
        true ->
            ok
    end,
    gen_server:cast(?SERVER, {measure, Max_iter, Delay, Measure_func, Do_filter}). % default_distance would be up to date here

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #state{default_Measure = -1.0}}.

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
handle_call(do_sonar_warmup, _From, State) ->
    Default_Measure = perform_sonar_warmup(),
    {reply, ok, State#state{default_Measure = Default_Measure}};
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
handle_cast({measure, Max_iter, Delay, Measure_func, Do_filter}, State) ->
    %spawn(?SERVER, make_measures, [0, Max_iter, Delay, Measure_func, Do_filter, State#state.default_Measure]),
    make_measures(0, Max_iter, Delay, Measure_func, Do_filter, State#state.default_Measure),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: state()}).
handle_info(_Request, State) ->
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

% only perform warmup when using real sonar
-spec perform_sonar_warmup() -> 
    Default_Measure :: {float(), integer()}.
perform_sonar_warmup() -> 
    perform_sonar_warmup_aux(0, 100, 50).

% todo: make maxiter/2 unused measures, then return median of next maxiter/2 measures? or osef?
-spec perform_sonar_warmup_aux(Iter :: integer(), Max_iter :: integer(), Delay :: integer()) ->
    Default_Measure :: {float(), integer()}.
perform_sonar_warmup_aux(Iter, Max_iter, Delay) -> % todo, selec mediane de toutes les mesures
    if
        Iter < Max_iter-1 ->
            pmod_maxsonar:get(),
            timer:sleep(Delay),
            perform_sonar_warmup_aux(Iter+1, Max_iter, Delay);
        Iter == Max_iter ->
            Measure = pmod_maxsonar:get(),
            Measure_timestamp = hera:get_timestamp(),
            {Measure, Measure_timestamp}
    end.



-spec make_measures(Iter :: integer(), Max_iter :: integer(), Delay :: integer(), Measure_func :: function(), Do_filter :: boolean(), Default_Measure :: float()) -> ok.
make_measures(Iter, Max_iter, Delay, Measure_func, Do_filter, Default_Measure) ->
    Measure = Measure_func(),
    Measure_timestamp = hera:get_timestamp(),
    Measure_str = io_lib:format("~.2f", [Measure]),
    io:format("measure: (~s) ~n", [Measure_str]), % print (todo: log)
    if % send current iter to other workers
        Do_filter == true ->
            hera_filter:filter({Measure, Measure_timestamp},  Iter, Default_Measure); 
        true ->
            Name = node(),
            hera:store_data(Name, Iter, Measure),
            hera:send(term_to_binary({Name, Iter, Measure}))
    end,
    if
        Iter < Max_iter-1 ->
            timer:sleep(Delay),
            make_measures(Iter+1, Max_iter, Delay, Measure_func, Do_filter, Default_Measure);
        true -> % no more measures to make
            io:format("measures done ~n", []),
            ok
    end.

