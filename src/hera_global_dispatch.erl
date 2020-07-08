%%%-------------------------------------------------------------------
%%% @author julien
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Jul 2020 6:48 PM
%%%-------------------------------------------------------------------
-module(hera_global_dispatch).
-author("julien").

-include("hera.hrl").
%% API
-export([start_link/2, init/2, dispatch/2]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(MeasurementName, GlobalName) ->
  global:register_name(GlobalName, Pid = spawn_link(?MODULE, init, [MeasurementName, GlobalName])),
  {ok, Pid}.

init(MeasurementName, GlobalName) ->
  dispatch(MeasurementName, GlobalName).

dispatch(MeasurementName, GlobalName) ->
  case get_and_remove_first(MeasurementName) of
    {{value, {Pid, Node}}, _} ->
      Pid ! {perform_measure, MeasurementName, GlobalName},
      receiver(Pid, MeasurementName, Node, 200); %% timeout value find by experiment the respond time
    _ -> timer:sleep(2000)
  end,
  dispatch(MeasurementName, GlobalName).

%%%===================================================================
%%% Internal functions
%%%===================================================================

receiver(From, MeasurementName, Node, Timeout) ->
  T1 = hera:get_timestamp(),
  receive
    {measure_done, MeasurementName, Node, continue} ->
      put_last({From, Node}, MeasurementName);
    %% If a node sends its response after Timeout, the response will arrive during another node measurement
    %% so we catch this message and adjust the timeout in order to receive the response of the current measurement
    {measure_done, MeasurementName, OtherNode, continue} ->
      put_last({get_pid_from_node_name(OtherNode), OtherNode}, MeasurementName),
      receiver(From, MeasurementName, Node, Timeout - (hera:get_timestamp() - T1));
    {measure_done, MeasurementName, _NodeName, stop} ->
      ok;
    SomethingElse ->
      logger:error("[Global_Serv] received message :~p~n", [SomethingElse])
  after Timeout ->
    logger:error("[Global_Serv] timeout when receiving measure confirmation")
  end.

get_and_remove_first(Name) ->
  gen_server:call({global, ?SYNC_PROC}, {get_and_remove_first, Name}).

put_last(Item, Name) ->
  gen_server:call({global, ?SYNC_PROC}, {put_last, Item, Name}).

get_pid_from_node_name(NodeName) ->
  gen_server:call({global, ?SYNC_PROC}, {get_pid, NodeName}).