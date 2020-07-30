-module(hera_filter).
-behaviour(gen_server).
-export([start_link/3, stop/1, filter/5]).
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
    previous_value :: {float(), integer()}, % {measure,timestamp}
    num_value :: integer(),
    num_filtered :: integer(),
    filtering_function :: fun((any(), any(), integer(), list(any())) -> boolean() | any()),
    type :: atom()
}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Name :: atom(), FilteringFunction :: fun((any(), any(), integer(), list(any())) -> boolean()), Type :: atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name, FilteringFunction, Type) ->
    FilterName = hera_utils:concat_atoms(filter_, Name),
    gen_server:start_link({local, FilterName}, ?MODULE, {FilteringFunction, Type}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

-spec(filter(Name :: atom(), Value :: {any(), integer()}, Iter :: integer(), UpperBound :: float(), AdditionalArgs :: list(any())) ->
    ok).
filter(Name, Value, Iter, UpperBound, AdditionalArgs)->
    FilterName = hera_utils:concat_atoms(filter_, Name),
    gen_server:cast(FilterName, {filter, Name, Value, Iter, UpperBound, AdditionalArgs}),
    ok.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
%% @doc Initializes the server
-spec(init(FilteringFunction :: fun((any(), any(), integer(), list(any())) -> boolean())) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init({FilteringFunction, Type}) ->
    {ok, #state{previous_value  = {-1.0, -1}, num_value = 0, num_filtered = 0, filtering_function = FilteringFunction, type = Type}}.

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
handle_cast({filter, Name, Value, Iter, UpperBound, AdditionalArgs}, State) ->
    {noreply, filter_value(Name, Value, Iter, UpperBound, AdditionalArgs, State)};
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
filter_value(Name, Value, 0, _UpperBound, _AddArgs, State) ->
    valid_measure(Name, 0, Value, State);
filter_value(Name, {CurrVal, ValTimestamp} = Value, Iter, UpperBound, AdditionalArgs, State = #state{previous_value = PreviousValue, num_value = NumValue, num_filtered = NumFiltered, filtering_function = FilteringFunction}) ->
    {PrevVal, PrevMeasureTimestamp} = PreviousValue,
    TimeDiff = abs(ValTimestamp - PrevMeasureTimestamp),
    case FilteringFunction(CurrVal, PrevVal, TimeDiff, UpperBound, AdditionalArgs) of
        true -> State#state{num_value =  NumValue+1, num_filtered = NumFiltered+1};
        false -> valid_measure(Name, Iter, Value, State);
        V when not is_boolean(V) -> valid_measure(Name, Iter, V, State);
        _ -> logger:error("[Filter] Bad return of the filtering function")
    end.

%% @private
%% @doc applies the procedure when the measure has successfully passed the filter
-spec(valid_measure(Name :: atom(), Iter :: integer(), Measure :: {integer()|float(), integer()}, State :: state())->
    State :: state()).
valid_measure(Name, Iter, {CurrVal, ValTimestamp} = Value, State = #state{num_value = NumValue, type = Type})->
    hera_sensors_data:store_data(Name, node(), Iter, CurrVal),
    SentVal = case Type of %% only send timestamp with a measurement
                    measure -> {CurrVal, ValTimestamp};
                    calc -> CurrVal
                end,
    hera:send(Type, Name, node(), Iter, SentVal),
    State#state{previous_value = Value, num_value = NumValue+1}.

