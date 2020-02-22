-module(hera_measure).
-behaviour(gen_server).
-export([start_link/2, stop/1]).
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
    measurement_func :: function(),
    delay :: integer(),
    id :: {binary(), atom()},
    iter :: integer()
}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Measurement_function :: function(), Delay :: integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Measurement_function, Delay) ->
    gen_server:start_link(?MODULE, {Measurement_function, Delay}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
%% @doc Initializes the server
-spec(init({Measurement_function :: function(), Delay :: integer()}) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init({Measurement_function, Delay}) ->
    Iter = 0,
    Id = {<<"measurements">>, state_orset},
    {ok, #state{measurement_func = Measurement_function, delay = Delay, id = Id, iter = Iter}, Delay}. % {ok, state, timeout}

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
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: state()}).
handle_info(timeout, State) ->
    Measure = State#state.measurement_func,
    %Measure = pmod_maxsonar:get() * 2.54,
    %Measure = hera:fake_sonar_get(),
    Name = node(),

    %with lasp
%%    {ok, Value} = lasp:query(Id),
%%    %io:format("set: (~p) ~n", [Value]),
%%    % S1, the set containing only values for Name
%%    S1 = sets:filter(fun(_Elem = {_Val, N}) -> N == Name end, Value),
%%    Length = sets:size(S1),
%%    if
%%        Length > 0 ->
%%            [{R1, Name}] = sets:to_list(S1),
%%            lasp:update(Id, {rmv, {R1, Name}}, self()),
%%            lasp:update(Id, {add, {Measure, Name}}, self());
%%        true ->
%%            lasp:update(Id, {add, {Measure, Name}}, self())
%%    end,

    %With udp multicast
    hera:store_data(Name, State#state.iter, Measure),
    hera:send(term_to_binary({Name, State#state.iter, Measure})),
    {noreply, State#state{iter = State#state.iter+1}, State#state.delay}.
%% We cannot use handle_info below: if that ever happens,
%% we cancel the timeouts (Delay) and basically zombify
%% the entire process. It's better to crash in this case.
%% handle_info(_Msg, State) ->
%%    {noreply, State}.

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