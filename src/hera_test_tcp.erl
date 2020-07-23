%%%-------------------------------------------------------------------
%%% @author julien
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jul 2020 9:46 PM
%%%-------------------------------------------------------------------
-module(hera_test_tcp).
-author("julien").

%% API
-export([create_tcp_serv/0, test_speed_tcp/1]).

test(Sock) ->
  receive
    {tcp, Sock, Data} ->
      gen_tcp:send(Sock, Data)
  end,
  test(Sock).

create_tcp_serv() ->
  {ok, Sock} = gen_tcp:listen(4402, [{active, true}, {reuseaddr, true}, inet, binary]),
  {ok, Sock2} = gen_tcp:accept(Sock),
  Pid = spawn(fun() -> test(Sock2) end),
  gen_tcp:controlling_process(Sock2, Pid).

send_msgs_tcp(Socks, Iter, Max) ->
  case Iter of
    Max -> terminated;
    _ ->
      Timestamp = hera:get_timestamp(),
      [gen_tcp:send(S, erlang:term_to_binary({Iter, Timestamp})) || S <- Socks], %% send the message to all nodes
      send_msgs_tcp(Socks, Iter+1, Max)
  end,
  timer:sleep(200).

test_speed_tcp(Max) ->
  Boards = [{169,254,16,1},{169,254,16,2},{169,254,16,3}],
  Socks = [gen_tcp:connect(B, 4402, [{active, true}, inet])|| B <- Boards], %% connect to all boards
  R = fun(Sock) -> %% when receive a packet from the right socket, log the iteration and timestamp
    receive
      {tcp, Sock, Data} ->
        {I, T} = erlang:binary_to_term(Data),
        logger:notice("~p ~p", [I, hera:get_timestamp() - T])
    end
  end,
  [gen_tcp:controlling_process(S, spawn(fun() -> R(S) end)) || S <- Socks], %% one process by socket
  send_msgs_tcp(Socks, 1, Max).