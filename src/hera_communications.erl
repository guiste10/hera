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

%% API
-export([start_link/0, init/0]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  register(?MODULE, Pid = spawn_link(?MODULE, init, [])),
  {ok, Pid}.

init() ->
  loop().

loop() ->
  receive
    {udp, _Sock, _IP, _InPortNo, Packet} ->
      handle_message(binary_to_term(Packet), os:type());
    {'EXIT', _ParentPid, shutdown} ->
      erlang:exit(shutdown);
    Other ->
      logger:error("[hera_communication] Wrong message received ~p~n", [Other])
  end,
  loop().

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

handle_message({propagate, Fun}, OsType) when OsType == {unix, rtems} ->
  catch Fun();

%% catch all non-conform messages
handle_message(_Other, _OsType) ->
  ok.