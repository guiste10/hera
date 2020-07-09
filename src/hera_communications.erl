%%%-------------------------------------------------------------------
%%% @author julien
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Jun 2020 5:13 PM
%%%-------------------------------------------------------------------
-module(hera_communications).
-author("julien").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

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
  {ok, #state{}}.

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
  handle_message(binary_to_term(Packet), os:type()),
  {noreply, State};
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

%% @private
%% @doc handles the messages received by udp multicast
%% measurements
handle_message({measure, Name, {Node, Iter, {Measure, _Timestamp}}}, OsType) when OsType == {unix, rtems} ->
  %% if it is a GRiSP board, don't log the measures, only save the most recent one
  %% in order to perform a computation
  hera_sensors_data:store_data(Name, Node, Iter, Measure);
handle_message({measure, Name, {Node, Iter, Measure}}, _OsType) ->
  %% if it is a computer, only log the measures, don't need to
  hera_sensors_data:log_measure(Name, Node, Iter, Measure);

%% calculations
handle_message({calc, Name, {Node, Iter, Res}}, OsType) when OsType =/= {unix, rtems}->
  hera_sensors_data:log_calculation(Name, Node, Iter, Res);

%% hello message
handle_message({hello, Node}, OsType) when OsType == {unix, rtems}->
  net_adm:ping(Node);

handle_message({propagate, Fun}, OsType) when OsType == {unix, rtems} ->
  Fun();

%% catch all non-conform messages
handle_message(_Other, _OsType) ->
  ok.