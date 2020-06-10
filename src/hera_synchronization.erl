%%%-------------------------------------------------------------------
%%% @author julien
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jun 2020 5:55 PM
%%%-------------------------------------------------------------------
-module(hera_synchronization).
-author("julien").

%% API
-export([start_link/0, init/0, loop/0]).
-export([make_measure_request/1]).

start_link() ->
  register(?MODULE, Pid = spawn_link(?MODULE, init, [])),
  Pid.

make_measure_request(Name) ->
  ?MODULE ! {make_measure_request, Name}.

init() ->
    loop().

loop() ->
  receive
    {make_measure_request, Name} ->
      hera_global_sync:make_measure_request(Name),
      loop();
    {perform_measure, Name, Pid} ->
      Resp = hera_measure:perform_single_measurement(Name),
      Pid ! {self(), measure_done, Resp},
      loop()
  end.
