%%%-------------------------------------------------------------------
%%% @author julien
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jun 2020 4:57 PM
%%%-------------------------------------------------------------------
-module(hera_global_sync).
-author("julien").

-behaviour(gen_server).

-include("hera.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  orders :: #{atom() => queue:queue()},
  nodes :: #{atom() => pid()}
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({global, ?SYNC_PROC}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{orders = maps:new(), nodes = #{}}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
%% when a node want to perform measures <Name>, we add this node to the corresponding queue
handle_call({make_measure, Name, Node}, _From = {Pid, _Ref}, State = #state{orders = M, nodes = Nodes}) ->
  case maps:is_key(Name, M) of
    false ->
      NewMap = M#{Name => Queue = queue:new()},
      {reply, ok, State#state{orders = NewMap#{Name => queue:in({Pid, Node}, Queue)}, nodes = Nodes#{Node => Pid}}};
    true ->
      Queue = maps:get(Name, M),
      {reply, ok, State#state{orders = M#{Name => queue:in({Pid, Node}, Queue)}, nodes = Nodes#{Node => Pid}}}
  end;
handle_call({get_and_remove_first, Name}, _From, State = #state{orders = M}) ->
  case maps:is_key(Name, M) of
    false -> {reply, not_yet_measurements_asked, State};
    true ->
      Queue = maps:get(Name, M),
      First = queue:out(Queue),
      {_, NewQueue} = First,
      {reply, First, State#state{orders = M#{Name => NewQueue}}}
  end;
handle_call({put_last, Item, Name}, _From, State = #state{orders = M}) ->
  Queue = maps:get(Name, M),
  {reply, ok, State#state{orders = M#{Name => queue:in(Item, Queue)}}};
handle_call({get_pid, NodeName}, _From, State = #state{nodes = Nodes}) ->
  {reply, maps:get(NodeName, Nodes), State};
handle_call({is_empty, Name}, _From, State = #state{orders = M}) ->
  case maps:is_key(Name, M) of
    false -> {reply, not_yet_measurements_asked, State};
    true ->
      {reply, queue:is_empty(maps:get(Name, M)), State}
  end;
handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State = #state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State = #state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================