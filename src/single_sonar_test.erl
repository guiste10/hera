-module(single_sonar_test).
-behaviour(gen_server).
-export([start_link/0, stop/1, perform_measures/4]).
-export([init/1, handle_call/3, handle_cast/2,
handle_info/2, code_change/3, terminate/2]).
-export([make_measures/1]).

%%====================================================================
%% Macros
%%====================================================================

-define(SERVER, ?MODULE).

%%====================================================================
%% Records
%%====================================================================

-record(state, {
    iter :: integer(),
    max_iter :: integer(),
    delay :: integer(),
    file :: file:io_device(),
    filename :: file:name_all(),
    func :: function()
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

%%--------------------------------------------------------------------
%% @doc
%% Perform measures
%%
%% @param Delay frequency of measurements
%% @param Max_iter number of measures to perform
%% @param File_name name of the file storing the data
%% @param Func the function to perform the measure
%%
%% @spec perform_measures(Delay :: integer(), Max_iter :: integer(), File_name :: file:name_all(), Func :: function()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec perform_measures(Delay :: integer(), Max_iter :: integer(), File_name :: file:name_all(), Func :: function()) -> ok.
perform_measures(Delay, Max_iter, File_name, Func) ->
    {ok, File} = file:open(File_name, [read, write]),
    gen_server:cast(?SERVER, {measure, Delay, Max_iter, File_name, File, Func}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    %{ok, File} = file:open(File_name, [read, write]),
    %Iter = 0,
    {ok, #state{iter = 0}}. % {ok, state, timeout}

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
handle_cast({measure, Delay, Max_iter, File_name, File, Func}, State) ->
    spawn(?SERVER, make_measures, [#state{delay = Delay, max_iter = Max_iter, file = File, filename = File_name, iter = 0, func = Func}]),
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
%%handle_info(timeout, State) ->
%%    Measure = pmod_maxsonar:get() * 2.54,
%%    Measure_str = io_lib:format("~.2f", [Measure]), % pour vrai sonar (float)
%%    %Measure = hera:fake_sonar_get(),
%%    %Measure_str = integer_to_list(Measure), % pour faux sonar (integer)
%%
%%    io:format("measure: (~s) ~n", [Measure_str]), % print
%%    Row = Measure_str ++ "\n",
%%    file:pwrite(State#state.file, eof, [Row]),
%%    if
%%        State#state.iter < State#state.max_iter-1 ->
%%            {noreply, State#state{iter = State#state.iter+1}, State#state.delay};
%%            true ->
%%                file:close(State#state.file),
%%               {noreply, State#state{iter = State#state.iter+1}}
%%               end.
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

%%====================================================================
%% Internal functions
%%====================================================================

-spec make_measures(State :: state()) -> ok.
make_measures(State) ->
    Measure = State#state.func,
    Measure_str = io_lib:format("~.2f", [Measure]), % pour vrai sonar (float)
    %Measure = hera:fake_sonar_get(),
    %Measure_str = integer_to_list(Measure), % pour faux sonar (integer)

    io:format("measure: (~s) ~n", [Measure_str]), % print
    Row = Measure_str ++ "\n",
    file:pwrite(State#state.file, eof, [Row]),
    if
        State#state.iter < State#state.max_iter-1 ->
            timer:sleep(State#state.delay),
            make_measures(State#state{iter = State#state.iter+1});
        true ->
            file:close(State#state.file),
            ok
    end.