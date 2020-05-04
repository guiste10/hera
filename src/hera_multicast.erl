%%%-------------------------------------------------------------------
%%% @author Julien Bastin <julien.bastin@student.uclouvain.be>
%%% @author Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% Multicast module that creates a multicast group where we can send and receive sensors data
%%% @end
%%% Created : 30. Jan 2020 12:45 PM
%%%-------------------------------------------------------------------
-module(hera_multicast).
-author("Julien Bastin <julien.bastin@student.uclouvain.be>, Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>").

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

%% @private
%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  io:format("multicast startlink~n"),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @private
stop(Pid) ->
  gen_server:call(Pid, stop).


%%--------------------------------------------------------------------
%% @doc
%% Initialize the multicast group
%%
%% @spec formation() -> ok
%% @end
%%--------------------------------------------------------------------
-spec formation() -> ok.
formation() ->
  io:format("Formation of mc cluster~n"),
  gen_server:cast(?SERVER , formation).

%% -------------------------------------------------------------------
%% @doc
%% Send a message over the multicast cluster
%%
%% @param Message the message to be send
%%
%% @spec send(Message :: term()) -> ok
%% @end
%% -------------------------------------------------------------------
-spec send(Message :: binary()) -> ok.
send(Message) ->
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
handle_cast(formation, State = #state{socket = S, controlling_process = Control}) ->
  Socket = case S of
    undefined ->
      Sock = open(),
      Sock;
    S ->
      S
  end,
  io:format("socket : ~p~n", [Socket]),
  ControllingProcess = case Control of
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
handle_cast({send_message, Message}, State = #state{socket = Socket}) ->
  %io:format("mc handle_cast send message~n"),
  case Socket of
    undefined ->
      io:format("Socket not yet started~n"),
      ok;
    Sock ->
      gen_udp:send(Sock, ?MULTICAST_ADDR, ?MULTICAST_PORT, Message)
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
terminate(_Reason, _State = #state{socket = Sock}) ->
  gen_udp:close(Sock),
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

%% @private
%% @doc Open a new udp multicast socket
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

%% @private
%% @doc Function that handles received udp multicast messages
receiver() ->
  receive
    {udp, _Sock, _IP, _InPortNo, Packet} ->
      case binary_to_term(Packet) of
        {measure, Name, {Node, Iter, Measure}} ->
          case os:type() of
            %% if it is a GRiSP board, don't log the measures, only save the most recent one
            %% in order to perform a computation
            {unix,rtems} ->
              hera:store_data(Name, Node, Iter, Measure);
            _ -> %% if it is a computer, only log the measures, don't need to
              hera:log_measure(Name, Node, Iter, Measure)
          end;
        {calc, Name, {Node, Iter, Res}} ->
          case os:type() of
            {unix, rtems} ->
              ok;
            _ ->
              hera:log_calculation(Name, Node, Iter, Res)
          end
      end,
      receiver();
    stop -> true;
    AnythingElse -> io:format("RECEIVED: ~p~n", [AnythingElse]),
      receiver()
  end.