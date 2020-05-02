%%%-------------------------------------------------------------------
%%% @author Julien Bastin <julien.bastin@student.uclouvain.be>
%%% @author Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% Module which includes all the functions related to our user case.
%%% Our user case consists in calculating the position of a move person in an empty room in realtime
%%% using GRiSP boards with Diligent pmod_maxsonar.
%%% @reference See <a href="https://grisp.org/" target="_blank">GRiSP site</a> and <a href="https://store.digilentinc.com/pmodmaxsonar-maxbotix-ultrasonic-range-finder/" target="_blank">Diligent site</a> for more information
%%% @end
%%% Created : 02. May 2020 2:22 AM
%%%-------------------------------------------------------------------

-module(hera_position).
-author("Julien Bastin <julien.bastin@student.uclouvain.be>, Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>").

%%====================================================================
%% Macros
%%====================================================================

-define(SERVER, ?MODULE).
-define(DATA_SERVER, hera_sensors_data).

%%====================================================================
%% Records
%%====================================================================

-record(state, {
    % dictionary contraining the last data of all nodes
    % with the form [{node_name, {seqnum, data}}]
    data :: dict:dict(string(), {integer(), integer() | float()}),
    calc_function :: function(),
    delay :: integer(),
    id :: {binary(), atom()},
    separation :: integer(),
    iter :: integer()
}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Calc_function :: function(), Delay :: integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Calc_function, Delay) ->
    io:format("position: startlink ~n"),
    gen_server:start_link(?MODULE, {Calc_function, Delay}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
%% @doc Initializes the server
-spec(init({Calc_function :: function(), Delay :: integer()}) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init({Calc_function, Delay}) ->
    Id = {<<"measurements">>,state_orset},
    %io:format("position: wait ~n"),
    %lasp:read(Id, {cardinality, 2}), % wait until set contains 2 measures
    Separation = 400,
    Iter = 0,
    io:format("position: init ~n"),
    Data = dict:new(),
    {ok, #state{data = Data, calc_function = Calc_function, delay = Delay, id = Id, separation = Separation, iter = Iter}, Delay}. % {ok, state, timeout}

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
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: state()}).
handle_info(timeout, State) ->
    %io:format("position: worker handling info ~n"),
    %with lasp
    %{ok, Values} = lasp:query(State#state.id),
    %Length = sets:size(Values),

    %without lasp
    Data = hera:get_data(),
    Values = dict:to_list(Data),
    Length = dict:size(Data),

    if  % assign ready2 to true if set contains 2 measures
        Length =:= 2 ->
            %[{R1, _},{R2, _}] = sets:to_list(Values), % [{measure, name}, ...]
            [{_Node1, {_Seqnum1, R1}}, {_Node2, {_Seqnum2, R2}}] = Values,
            R1Sq = math : pow ( R1 , 2) ,
            R2Sq = math : pow ( R2 , 2) ,
            S2 = 2 * State#state.separation ,
            SSq = math : pow ( State#state.separation , 2) ,
            X = ( R1Sq - R2Sq + SSq ) / S2 ,
            Helper = R1Sq - math : pow (X , 2),
            if
                Helper < 0 ->
                    %ok;
                    io:format("position: not definable: square root of neg number ~n");
                true ->
                    Y1 = math : sqrt ( Helper ) ,
                    Y2 = - Y1,
                    io:format("position: (~p, ~p) or (~p, ~p) ~n", [X, Y1, X, Y2])
            end;
        true ->
            %ok
            io:format("position: not definable: not 2 available measures : ~p~n", [Values])
    end,
    {noreply, State#state{iter = State#state.iter + 1}, State#state.delay}.
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