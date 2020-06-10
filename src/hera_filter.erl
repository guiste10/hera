-module(hera_filter).
-behaviour(gen_server).
-export([start_link/0, stop/1, filter/5]).
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
    previous_measure :: {float(), integer()}, % {measure,timestamp}
    num_measures :: integer(),
    num_filtered :: integer()
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

stop(Pid) ->
    gen_server:call(Pid, stop).

-spec(filter(Measure :: {float(), integer()}, Iter :: integer(), DefaultMeasure :: {float(), integer()}, Name :: atom(), UpperBound :: float()) ->
    ok).
filter(Measure, Iter, DefaultMeasure, Name, UpperBound)->
    gen_server:cast(?SERVER, {filter, Measure, Iter, DefaultMeasure, Name, UpperBound}),
    ok.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
%% @doc Initializes the server
-spec(init([]) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #state{previous_measure = {-1.0, -1}, num_measures = 0, num_filtered = 0}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: state()) ->
    {reply, Reply :: term(), NewState :: state()} |
    {reply, Reply :: term(), NewState :: state(), timeout() | hibernate} |
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: state()} |
    {stop, Reason :: term(), NewState :: state()}).
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
handle_cast({filter, Measure, Iter, DefaultMeasure, Name, UpperBound}, State) ->
    State2 = filter_measure(Measure, Iter, DefaultMeasure, Name, UpperBound, State),
    {noreply, State2};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: state()}).
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
%% @doc applies a filter on the received measure. 
% suppose at first call that previous_measure = default distance as in hera_measure:perform_sonar_warmup_aux()
-spec(filter_measure(Measure :: {integer()|float(), integer}, Iter :: integer(), DefaultMeasure :: {float(), integer()}, Name :: atom(), State :: state(), UpperBound :: float())->
    State :: state()).
filter_measure(Measure, 0, _DefaultMeasure, Name, _UpperBound, State)-> % don't filter when there is no previous measure
    valid_measure(Name, 0, Measure, State);
filter_measure({CurrMeasureVal, MeasureTimestamp} = Measure, Iter, {DefaultMeasureVal, _}, Name, UpperBound, State)->
    {PrevMeasureVal, PrevMeasureTimestamp} = State#state.previous_measure,
    TimeDiff = abs(MeasureTimestamp - PrevMeasureTimestamp),
    DoFilter = case Name of 
        sonar -> filter_sonar(PrevMeasureVal, CurrMeasureVal, DefaultMeasureVal, UpperBound, TimeDiff);  %UpperBound = 0.28, % = 10.0(km/h)/35.714 cm/ms
        _ -> abs(CurrMeasureVal - PrevMeasureVal) > UpperBound*TimeDiff
    end,
    if 
        DoFilter == true -> % filter out measure
            State#state{num_measures = State#state.num_measures+1, num_filtered = State#state.num_filtered+1};
        true ->  % don't filter out
            valid_measure(Name, Iter, Measure, State)
    end.

%% @private
%% @doc applies the procedure when the measure has sucessfully passed the filter
-spec(valid_measure(Name :: atom(), Iter :: integer(), Measure :: {integer()|float(), integer()}, State :: state())->
    State :: state()).
valid_measure(Name, Iter, {CurrMeasureVal, MeasureTimestamp} = Measure, State)->
    hera:store_data(Name, node(), Iter, CurrMeasureVal),
    hera:send(measure, Name, node(), Iter, {CurrMeasureVal, MeasureTimestamp}),
    State#state{previous_measure = Measure, num_measures = State#state.num_measures+1}.


%% @private
%% @doc used for sonar measurements only. 
%% It returns true if the sonar measure has to be filtered out
-spec(filter_sonar(PrevMeasureVal :: float(), CurrMeasureVal :: float(), DefaultMeasureVal :: float(), UpperBound :: float(), TimeDiff :: integer())->
    State :: state()).
filter_sonar(PrevMeasureVal, CurrMeasureVal, DefaultMeasureVal, UpperBound, TimeDiff) -> 
    PrevIsBackDist = is_background_dist(PrevMeasureVal, DefaultMeasureVal),
    IsDefDist = is_background_dist(CurrMeasureVal, DefaultMeasureVal),
    IsDefDist orelse
    (PrevIsBackDist == false andalso IsDefDist == false andalso
    abs(CurrMeasureVal - PrevMeasureVal) > UpperBound*TimeDiff). % 0.28*(100=TimeDiff) = 0.28*TimeDiff cm/TimeDiff ms


%% @private
%% @doc used for sonar measurements only. 
%% It returns true if the measure is equal to or greater than the distance measured during the warmup phase = the background distance
-spec(is_background_dist(MeasureVal :: float(), DefaultMeasureVal :: float())->
    boolean()).
is_background_dist(MeasureVal, DefaultMeasureVal)->
    if
        DefaultMeasureVal - 2.54 =< MeasureVal ->
            true;
        true ->
            false
    end.
