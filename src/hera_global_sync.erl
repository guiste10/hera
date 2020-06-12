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
-export([dispatch/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  orders :: #{atom() => queue:queue()}
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
  {ok, #state{orders = maps:new()}}.

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
handle_call({make_measure, Name}, _From = {Pid, _Ref}, State = #state{orders = M}) ->
  case maps:is_key(Name, M) of
    false ->
      GlobalName = hera_utils:concat_atoms(dispatch_, Name),
      {P, _R} = spawn_opt(?SERVER, dispatch, [Name, GlobalName], [monitor]),
      global:register_name(GlobalName, P),
      NewMap = M#{Name => Queue = queue:new()},
      {reply, ok, State#state{orders = NewMap#{Name => queue:in(Pid, Queue)}}};
    true ->
      Queue = maps:get(Name, M),
      {reply, ok, State#state{orders = M#{Name => queue:in(Pid, Queue)}}}
  end;
handle_call({get_and_remove_first, Name}, _From, State = #state{orders = M}) ->
  Queue = maps:get(Name, M),
  First = queue:out(Queue),
  {_, NewQueue} = First,
  {reply, First, State#state{orders = M#{Name => NewQueue}}};
handle_call({put_last, Item, Name}, _From, State = #state{orders = M}) ->
  Queue = maps:get(Name, M),
  {reply, ok, State#state{orders = M#{Name => queue:in(Item, Queue)}}};
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

get_and_remove_first(Name) ->
  gen_server:call({global, ?SYNC_PROC}, {get_and_remove_first, Name}).

put_last(Item, Name) ->
  gen_server:call({global, ?SYNC_PROC}, {put_last, Item, Name}).

dispatch(MeasurementName, GlobalName) ->
  case get_and_remove_first(MeasurementName) of
    {empty, _} -> dispatch(MeasurementName, GlobalName);
    {{value, From}, _} ->
      From ! {perform_measure, MeasurementName, GlobalName},
      receive
        {measure_done, MeasurementName, continue} ->
          put_last(From, MeasurementName),
          dispatch(MeasurementName, GlobalName);
        {measure_done, MeasurementName, stop} ->
          dispatch(MeasurementName, GlobalName);
        SomethingElse ->
          logger:error("[Global_Serv] received message :~p~n", [SomethingElse]),
          dispatch(MeasurementName, GlobalName)
      after 100 ->
        logger:error("Global_Serv] timeout when receiving measure confirmation~n"),
        dispatch(MeasurementName, GlobalName)
      end
  end.