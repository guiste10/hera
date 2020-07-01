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
  syn:register(GlobalName, Pid = spawn_link(?MODULE, init, [MeasurementName, GlobalName])),
  {ok, Pid}.

init(MeasurementName, GlobalName) ->
  dispatch(MeasurementName, GlobalName).

dispatch(MeasurementName, GlobalName) ->
  case get_and_remove_first(MeasurementName) of
    not_yet_measurements_asked ->
      timer:sleep(2000),
      dispatch(MeasurementName, GlobalName);
    {empty, _} -> dispatch(MeasurementName, GlobalName);
    {{value, From}, _} ->
      From ! {perform_measure, MeasurementName, GlobalName},
      receive
        {measure_done, MeasurementName, continue} ->
          put_last(From, MeasurementName),
          dispatch(MeasurementName, GlobalName);
        {measure_done, MeasurementName, stop} ->
          dispatch(MeasurementName, GlobalName);
        SomethingElse ->
          logger:error("[Global_Serv] received message :~p~n", [SomethingElse]),
          dispatch(MeasurementName, GlobalName)
      after 100 ->
        logger:error("[Global_Serv] timeout when receiving measure confirmation~n"),
        dispatch(MeasurementName, GlobalName)
      end
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_and_remove_first(Name) ->
  gen_server:call({via, syn, ?SYNC_PROC}, {get_and_remove_first, Name}).

put_last(Item, Name) ->
  gen_server:call({via, syn, ?SYNC_PROC}, {put_last, Item, Name}).