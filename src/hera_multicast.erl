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

%% API
-export([start_link/0, send/5, send/1, hello/0, init/0]).

%%====================================================================
%% Macros
%%====================================================================

-define(SERVER, ?MODULE).
-define(MULTICAST_ADDR, {224,0,2,254}).
-define(MULTICAST_PORT, 62476).

%%%===================================================================
%%% API
%%%===================================================================

%% @private
%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  register(?MODULE, Pid = spawn_link(?MODULE, init, [])),
  {ok, Pid}.

%% -------------------------------------------------------------------
%% @doc
%% Send a data over the multicast cluster
%%
%% @param Message_type The type of the message to be sent, either calc or measure
%% @param Name The name of the sent data
%% @param Node The node which send the message
%% @param Seqnum The sequence number of the data
%% @param Data The data to be sent
%%
%% @spec send(Message_type :: calc | measure, Name :: atom(), Node :: atom(), Seqnum :: integer(), Data :: term()) -> ok
%% @end
%% -------------------------------------------------------------------
-spec send(Message_type :: calc | measure, Name :: atom(), Node :: atom(), Seqnum :: integer(), Data :: term()) -> ok.
send(Message_type, Name, Node, Seqnum, Data) ->
  ?SERVER ! {send_message, term_to_binary({Message_type, Name, {Node, Seqnum, Data}})}.

%% -------------------------------------------------------------------
%% @doc
%% Send a message the multicast cluster
%%
%% @param Message The message to be sent
%%
%% @spec send(Message_type :: calc | measure, Name :: atom(), Node :: atom(), Seqnum :: integer(), Data :: term()) -> ok
%% @end
%% -------------------------------------------------------------------
-spec send(Message :: term()) -> any().
send(Message) ->
 ?SERVER ! {send_message, term_to_binary(Message)}.

init() ->
  {_Pid, _Ref} = spawn_opt(?SERVER, hello, [], [monitor]),
  Socket = open_socket(),
  loop(Socket).

loop(Socket) ->
  receive
    {send_message, Message} ->
      send_message(Socket, Message)
  end,
  loop(Socket).
%%%===================================================================
%%% Internal functions
%%%===================================================================

send_message(Socket, Message) ->
  case Socket of
    undefined ->
      logger:notice("[hera_multicast] socket not yet opened.");
    Sock ->
      gen_udp:send(Sock, ?MULTICAST_ADDR, ?MULTICAST_PORT, Message)
  end.

open_socket() ->
  Socket = open(),
  Pid = whereis(hera_communications),
  ok = gen_udp:controlling_process(Socket, Pid),
  Socket.

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

hello() ->
  hera_multicast:send({hello, node()}),
  timer:sleep(2000),
  hello().
