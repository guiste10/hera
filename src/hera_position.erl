-module(hera_position).
-behaviour(gen_server).
-export([start_link/1, stop/1, store_data/3]).
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
    % dictionary contraining the last data of all nodes
    % with the form [{node_name, {seqnum, data}}]
    data :: dict:dict(string(), {integer(), integer() | float()}),
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
-spec(start_link(Delay :: integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Delay) ->
    io:format("position: startlink ~n"),
    gen_server:start_link(?MODULE, Delay, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

%%--------------------------------------------------------------------
%% @doc
%% Update the state with a new value of the data
%% @end
%%--------------------------------------------------------------------
-spec(store_data(Node :: string(), Seqnum :: integer(), Data :: integer() | float()) -> ok).
store_data(Node, Seqnum, Data) ->
    io:format("store data~n"),
    gen_server:call(?SERVER, {store_data, {Node, Seqnum, Data}}).
%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init(Delay) ->
    Id = {<<"measurements">>,state_orset},
    %io:format("position: wait ~n"),
    %lasp:read(Id, {cardinality, 2}), % wait until set contains 2 measures
    Separation = 400,
    Iter = 0,
    io:format("position: init ~n"),
    Data = dict:new(),
    {ok, #state{data = Data, delay = Delay, id = Id, separation = Separation, iter = Iter}, Delay}. % {ok, state, timeout}

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
handle_call({store_data, {Node, Seqnum, Data}}, _From, State) ->
    io:format("store_data~n"),
    Dict2 = case dict:find(Node, State#state.data) of
                {ok, {S, _Data}} ->
                    io:format("S : ~p, Data : ~p~n", [S, _Data]),
                    if
                        S < Seqnum ->
                            dict:store(Node, {Seqnum, Data}, State#state.data);
                        true ->
                            State#state.data
                    end;
                error ->
                    io:format("store data first ~n"),
                    dict:store(Node, {Seqnum, Data}, State#state.data)
            end,
    io:format("Dict2 : ~p~n", [Dict2]),
    {noreply, State#state{data = Dict2}};
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
    Values = dict:to_list(State#state.data),
    Length = dict:size(State#state.data),

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