-module(hera_measure).
-behaviour(gen_server).

-include("hera.hrl").

-export([start_link/4, stop/1]).
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
    iter :: integer()
}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Name :: atom(), Measurement_func :: function(), Func_args :: list(any()), Delay :: integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name, Measurement_func, Args, Delay) ->
    gen_server:start_link(?MODULE, {Name, Measurement_func, Args, Delay}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
%% @doc Initializes the server
-spec(init({Name :: atom(), Measurement_func :: function(), Args :: list(any()), Delay :: integer()}) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init({Name, Measurement_func, Args, Delay}) ->
    {ok, #state{name = Name, measurement_func = Measurement_func, func_args = Args, delay = Delay, iter = 0}, Delay}. % {ok, state, timeout}

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
handle_info(timeout, State = #state{name = Name, measurement_func = Func, func_args = Args, iter = Iter, delay = Delay}) ->
    Measure = erlang:apply(Func, Args),
    hera:store_data(Name, node(), Iter, Measure),
    hera:send({measure, Name, {node(), Iter, Measure}}),
    {noreply, State#state{iter = Iter+1 rem ?MAX_SEQNUM}, Delay}.
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