-module(hera_serv).
-behaviour(gen_server).
-export([start_link/4, run/2, sync_queue/2, async_queue/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
code_change/3, terminate/2]).

%% The worker supervisor is started dynamically!
-define(SPEC(MFA),
    {worker_sup, % name
     {hera_worker_sup, start_link, [MFA]}, % MFA = module,function,argument of the worker to run
     temporary, 10000, supervisor,[hera_worker_sup]}).

-record(state, {limit=0, sup, refs, queue=queue:new()}).

-type state() :: state().

%%====================================================================
%% server API
%%====================================================================
 
% starts the server
-spec(start_link(Name :: atom(), Limit :: integer(), Supervisor :: atom(), MFA :: tuple()) ->
    {ok , Pid :: pid()} | ignore | {error , Reason :: term()}).
start_link(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
    io:format("hera_serv started!~n"),
    gen_server:start_link({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

-spec(run(Name :: atom(), Arguments :: list()) ->  gen_server:reply()).
run(Name, Args) ->
    gen_server:call(Name, {run, Args}).

-spec(sync_queue(Name :: atom(), Arguments :: list()) ->  gen_server:reply()).
sync_queue(Name, Args) ->
    gen_server:call(Name, {sync, Args}, infinity).

-spec(async_queue(Name :: atom(), Arguments :: list()) ->  ok). 
async_queue(Name, Args) ->
    gen_server:cast(Name, {async, Args}).
 
-spec(stop(atom()) -> gen_server:reply()).
stop(Name) ->
    gen_server:call(Name, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

% initializes the server
-spec (init({Limit :: integer(), MFA :: tuple(), Supervisor :: term()}) ->
    {ok , State :: state()}).
init({Limit, MFA, Sup}) ->
    %% We need to find the Pid of the worker supervisor from here,
    %% but alas, this would be calling the supervisor while it waits for us!
    self() ! {start_worker_supervisor, Sup, MFA},
    {ok, #state{limit=Limit, refs=gb_sets:empty()}}.

-spec(handle_info(Request :: term(), State :: state()) ->
    {noreply , State :: state()}).
handle_info({'DOWN', Ref, process, _Pid, _}, S = #state{refs=Refs}) ->
    io:format("received down msg~n"),
    case gb_sets:is_element(Ref, Refs) of
        true -> % make sure the 'DOWN' message we get comes from a worker of ours
            handle_down_worker(Ref, S);
        false -> %% Not our responsibility
            {noreply, S}
    end;
handle_info({start_worker_supervisor, Sup, MFA}, S = #state{}) ->
    {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)), % ask the supervisor "sup" to start a child
    link(Pid),
    {noreply, S#state{sup=Pid}};
handle_info(Msg, State) ->
    io:format("Unknown msg: ~p~n", [Msg]),
    {noreply, State}.

% Whenever there are places left in the pool (the original limit N being 
% decided by the programmer adding the pool in the first place), we accept 
% to start the worker. We then set up a monitor to know when it's done, store 
% all of this in our state, decrement the counter and off we go.
% In the case no space is available, we simply reply with noalloc.
-spec(handle_call(Request :: term() , From :: {pid() , Tag :: term()} , State :: state()) -> 
    {reply, NewState :: state()} |
    {noreply, NewState :: state()}).
handle_call({run, Args}, _From, S = #state{limit=N, sup=Sup, refs=R}) when N > 0 ->
    io:format("start worker~n"),
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {reply, {ok,Pid}, S#state{limit=N-1, refs=gb_sets:add(Ref,R)}};
handle_call({run, _Args}, _From, S=#state{limit=N}) when N =< 0 ->
    {reply, noalloc, S};
handle_call({sync, Args}, _From, S = #state{limit=N, sup=Sup, refs=R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {reply, {ok,Pid}, S#state{limit=N-1, refs=gb_sets:add(Ref,R)}};
handle_call({sync, Args},  From, S = #state{queue=Q}) ->
    {noreply, S#state{queue=queue:in({From, Args}, Q)}};   % add to queue 
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
    handle_call(_Msg, _From, State) ->
    {noreply, State}.


-spec(handle_cast(Request :: term() , State :: state())->
    {noreply, NewState :: state()}).
handle_cast({async, Args}, S=#state{limit=N, sup=Sup, refs=R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {noreply, S#state{limit=N-1, refs=gb_sets:add(Ref,R)}};
handle_cast({async, Args}, S=#state{limit=N, queue=Q}) when N =< 0 ->
    {noreply, S#state{queue=queue:in(Args,Q)}};
    %% Not going to explain this one!
handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
        
terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec(handle_down_worker(Worker :: reference(), State :: state())->
    {noreply, NewState :: state()}).
handle_down_worker(Ref, S = #state{limit=L, sup=Sup, refs=Refs}) ->
    case queue:out(S#state.queue) of
        {{value, {From, Args}}, Q} -> % queue element came from sync_queue
            {ok, Pid} = supervisor:start_child(Sup, Args),
            NewRef = erlang:monitor(process, Pid),
            NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref,Refs)),
            gen_server:reply(From, {ok, Pid}), % finally reply ok
            {noreply, S#state{refs=NewRefs, queue=Q}};
        {{value, Args}, Q} -> % queue element came from async_queue
            {ok, Pid} = supervisor:start_child(Sup, Args),
            NewRef = erlang:monitor(process, Pid),
            NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref,Refs)),
            {noreply, S#state{refs=NewRefs, queue=Q}};
        {empty, _} -> % empty queue
            {noreply, S#state{limit=L+1, refs=gb_sets:delete(Ref,Refs)}}
    end.