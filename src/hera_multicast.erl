%%%-------------------------------------------------------------------
%%% @author julien bastin
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2020 12:45 PM
%%%-------------------------------------------------------------------
-module(hera_multicast).
-author("julien").

-behaviour(gen_server).

%% API
-export([start_link/0, stop/1, formation/0, send/1]).
-export([receiver/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%%====================================================================
%% Macros
%%====================================================================

-define(SERVER, ?MODULE).
-define(MULTICAST_ADDR, {224,0,2,254}).
-define(MULTICAST_PORT, 62476).

%%====================================================================
%% Records
%%====================================================================

-record(state, {
  controlling_process :: {pid(), reference()},
  socket :: gen_udp:socket()
}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  io:format("multicast startlink~n"),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop(Pid) ->
  gen_server:call(Pid, stop).


%%--------------------------------------------------------------------
%% @doc
%% Formation signal call
%%
%% @spec formation() -> ok
%% @end
%%--------------------------------------------------------------------
-spec formation() -> ok.
formation() ->
  io:format("Formation of mc cluster~n"),
  gen_server:cast(?SERVER , formation).

%%--------------------------------------------------------------------
%% @doc
%% Send a message over the multicast cluster
%%
%% @spec send(Message :: binary()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec send(Message :: binary()) -> ok.
send(Message) ->
  %io:format("Send a message~n"),
  gen_server:cast(?SERVER, {send_message, Message}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{
    controlling_process = {undefined, undefined},
    socket = undefined
  }}.

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
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: state()) ->
  {noreply, NewState :: state()} |
  {noreply, NewState :: state(), timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: state()}).
handle_cast(formation, State) ->
  Socket = case State#state.socket of
    undefined ->
      Sock = open(),
      Sock;
    S ->
      S
  end,
  io:format("socket : ~p~n", [Socket]),
  ControllingProcess = case State#state.controlling_process of
    {undefined, undefined} ->
      {Pid, Ref} = spawn_opt(?SERVER, receiver, [], [monitor]),
      io:format("~nFirst Pid: ~p~nRef: ~p~n",[Pid, Ref]),
      ok = gen_udp:controlling_process(Socket, Pid),
      {Pid, Ref};
    {Pid, Ref} ->
      io:format("~nPid: ~p~nRef: ~p~n",[Pid, Ref]),
      {Pid, Ref}
  end,
  {noreply, State#state{controlling_process = ControllingProcess, socket = Socket}};
handle_cast({send_message, Message}, State) ->
  %io:format("mc handle_cast send message~n"),
  case State#state.socket of
    undefined ->
      io:format("Socket not yet started~n"),
      ok;
    Sock ->
      Ret = gen_udp:send(Sock, ?MULTICAST_ADDR, ?MULTICAST_PORT, Message),
      io:format("Send return message : ~p~n", [Ret])
  end,
  {noreply, State}.


%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: state()) ->
  {noreply, NewState :: state()} |
  {noreply, NewState :: state(), timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: state()}).
handle_info(_Request, State) ->
  %io:format("mc handle_info, request : ~p~n", [_Request]),
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: state()) -> term()).
terminate(_Reason, _State) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: state(),
    Extra :: term()) ->
  {ok, NewState :: state()} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

open() ->
  {ok, Addrs} = inet:getifaddrs(),
  % get address of the wifi interface
  OwnAddr = hd([
    Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
    size(Addr) == 4, Addr =/= {127,0,0,1}
  ]),
  {ok, Sock} = gen_udp:open(?MULTICAST_PORT, [
    binary,
    inet,
    {active, true},
    {multicast_if, OwnAddr}, %specify the network interface to use to send multicast
    {multicast_loop, false}, %no return of the packets
    {reuseaddr, true},
    {add_membership, {?MULTICAST_ADDR, OwnAddr}} %join a multicast group and use the specified network interface
  ]),
  Sock.

stop_mc({Sock, Pid}) ->
  gen_udp:close(Sock),
  Pid ! stop.

receiver() ->
  receive
    {udp, _Sock, IP, InPortNo, Packet} ->
      T = binary_to_term(Packet),
      {Node, Iter, Measure} = T,
      hera:store_data(Node, Iter, Measure),
      %io:format("~n~nFrom: ~p~nPort: ~p~nData:~p~n", [IP,InPortNo,Packet]),
      io:format("~nNode: ~p~nMeasure: ~p~nIter: ~p~n", [Node, Measure, Iter]),
      receiver();
    stop -> true;
    AnythingElse -> io:format("RECEIVED: ~p~n", [AnythingElse]),
      receiver()
  end.