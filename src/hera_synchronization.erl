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

-include("hera.hrl").
%% API
-export([start_link/0, init/0, loop/0]).
-export([make_measure_request/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  register(?MODULE, Pid = spawn_link(?MODULE, init, [])),
  {ok, Pid}.

make_measure_request(Name) ->
  ?MODULE ! {make_measure_request, Name}.

init() ->
    loop().

loop() ->
  receive
    {make_measure_request, Name} ->
      started = ensure_global_sync_started(),
      gen_server:call({via, syn, ?SYNC_PROC}, {make_measure, Name}),
      loop();
    {perform_measure, Name, GlobalName} ->
      Resp = hera_measure:perform_single_measurement(Name),
      syn:whereis(GlobalName) ! {measure_done, Name, Resp},
      loop();
    SomethingElse ->
      logger:error("[Synchronization] received message: ~p~n", [SomethingElse]),
      loop()
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

ensure_global_sync_started() ->
  case syn:whereis(?SYNC_PROC) of
    undefined ->
      timer:sleep(1000),
      ensure_global_sync_started();
    _Pid -> started
  end.