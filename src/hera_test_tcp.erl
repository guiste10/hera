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

send_msgs_tcp(Sock, Iter, Max, B) ->
  case Iter of
    Max -> terminated;
    _ ->
      {ok, Data} = B(),
      logger:notice("~p", [hera:get_timestamp() - list_to_integer(Data)]),
      send_msgs_tcp(Sock, Iter+1, Max, B)
  end,
  timer:sleep(200).

test_speed_tcp(Max) ->
  {ok, Sock} = gen_tcp:connect({169,254,16,1}, 4402, [{active, false}, inet]),
  B = fun() -> gen_tcp:send(Sock, erlang:integer_to_binary(hera:get_timestamp())), gen_tcp:recv(Sock, 0) end,
  send_msgs_tcp(Sock, 1, Max, B).