%%%-------------------------------------------------------------------
%%% @author julien
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jun 2020 4:31 PM
%%%-------------------------------------------------------------------
-module(hera_communications).
-author("julien").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).
-export([check_alive_nodes/0]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  ets:new(alive_nodes, [named_table, public, ordered_set]),
  ets:new(measurement_phase_nodes, [public, ordered_set, named_table]),
  {_Pid, _Ref} = spawn_opt(?SERVER, check_alive_nodes, [], [monitor]),
  {ok, #state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: ter()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
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
handle_info({udp, _Sock, _IP, _InPortNo, Packet}, State) ->
  handle_message(binary_to_term(Packet)),
  {noreply, State};
handle_info(_Info, State) ->
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

handle_message({measure, Name, {Node, Iter, Measure}}) ->
  case os:type() of
    %% if it is a GRiSP board, don't log the measures, only save the most recent one
    %% in order to perform a computation
    {unix,rtems} ->
      hera:store_data(Name, Node, Iter, element(1, Measure));
    _ -> %% if it is a computer, only log the measures, don't need to
      hera:log_measure(Name, Node, Iter, Measure)
  end;
handle_message({calc, Name, {Node, Iter, Res}}) ->
  case os:type() of
    {unix, rtems} ->
      ok;
    _ ->
      hera:log_calculation(Name, Node, Iter, Res)
  end;
handle_message({keep_alive, Node}) ->
  ets:insert(alive_nodes, {Node, hera:get_timestamp()});
handle_message({measurement_phase, Name, Phase, Node}) ->
  ets:insert(measurement_phase_nodes, {{Name, Node}, Phase}),
  update_sync_phase(Name).



%% @private
%% @doc check every 500 ms if other nodes are alive
check_alive_nodes() ->
  %% If we have not receive a keep_alive message from a node during the past 500 ms,
  %% the node is removed from the alive_nodes list
  [Dead_Nodes] = [ets:take(alive_nodes, N) || {N, T} <- ets:tab2list(alive_nodes), hera:get_timestamp()-T > 500],
  %% remove dead nodes from the measurement_phase_node table
  [ets:match_delete(measurement_phase_nodes, {{'_', N}, '_'}) || N <- Dead_Nodes],
  timer:sleep(500),
  check_alive_nodes().

update_sync_phase(Name) ->
  L = length(ets:tab2list(measurement_phase_nodes)),
  if
    L+1 > 1 -> hera_measure:update_sync_phase(Name, true);
    true -> hera_measure:update_sync_phase(Name, false)
  end.