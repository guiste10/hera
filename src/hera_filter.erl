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

-spec(filter(Measure :: {float(), integer()}, Iter :: integer(), Default_measure :: {float(), integer()}, Name :: atom(), UpperBound :: float()) ->
    ok).
filter(Measure, Iter, Default_measure, Name, UpperBound)->
    gen_server:cast(?SERVER, {filter, Measure, Iter, Default_measure, Name, UpperBound}),
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
handle_cast({filter, Measure, Iter, Default_measure, Name, UpperBound}, State) ->
    State2 = filter_measure(Measure, Iter, Default_measure, Name, UpperBound, State),
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


% suppose at first call that previous_measure = default distance as in hera_measure:perform_sonar_warmup_aux()
-spec(filter_measure(Measure :: {float(), integer}, Iter :: integer(), Default_measure :: {float(), integer()}, Name :: atom(), State :: state(), UpperBound :: float())->
    State :: state()).
filter_measure({Curr_measure_val, Measure_timestamp} = Measure, Iter, {Default_measure_val, _} = Default_measure, Name, UpperBound, State)->
    {Prev_measure_val, Prev_measure_timestamp} = get_previous_measure(Iter, Default_measure, State#state.previous_measure),
    Time_diff = abs(Measure_timestamp - Prev_measure_timestamp),
    DoFilter = case Name of 
        sonar -> filter_sonar(Prev_measure_val, Curr_measure_val, Default_measure_val, UpperBound, Time_diff);  %UpperBound = 0.28, % = 10.0(km/h)/35.714 cm/ms
        _ -> abs(Curr_measure_val - Prev_measure_val) > UpperBound*Time_diff
    end,
    if % if true then filter
        DoFilter == true ->
            State#state{num_measures = State#state.num_measures+1, num_filtered = State#state.num_filtered+1}; % keep old previous measure
        true ->
            % don't filter out
            hera:store_data(Name, node(), Iter, Curr_measure_val),
            hera:send(measure, Name, node(), Iter, {Curr_measure_val, Measure_timestamp}),
            State#state{previous_measure = Measure, num_measures = State#state.num_measures+1} % don't increment numfiltered
    end.

-spec(is_default_measure(Measure :: float(), Default_measure :: {float(), integer()})->
    boolean()).
is_default_measure(Measure, Default_measure)->
    if
        Default_measure - 2.54 =< Measure andalso Measure =< Default_measure + 2.54 ->
            true;
        true ->
            false
    end.

get_previous_measure(Iter, DefaultMeasure, PreviousMeasure) ->
    case Iter of
        0 -> DefaultMeasure;  % first performed measure after warmup
        _ -> PreviousMeasure
    end.

filter_sonar(Prev_measure_val, Curr_measure_val, Default_measure_val, UpperBound, Time_diff) ->
    Prev_is_def_dist = is_default_measure(Prev_measure_val, Default_measure_val),
    Is_def_dist = is_default_measure(Curr_measure_val, Default_measure_val),
    Curr_measure_val > Default_measure_val + 2.54 orelse 
    (Prev_is_def_dist == false andalso Is_def_dist == false andalso
    abs(Curr_measure_val - Prev_measure_val) > UpperBound*Time_diff). % 0.28*(100=Time_diff) = 0.28*Time_diff cm/Time_diff ms
