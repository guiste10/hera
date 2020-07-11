%%%-------------------------------------------------------------------
%%% @author Julien Bastin <julien.bastin@student.uclouvain.be>
%%% @author Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% Hera public api
%%% @end
%%%
%%%-------------------------------------------------------------------
-module(hera).
-author("Julien Bastin <julien.bastin@student.uclouvain.be>, Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>").

-behavior(application).

-include("hera.hrl").

%% API
-export([launch_app/3]).
-export([launch_app/0]).
-export([fake_sonar_get/0]).
-export([send/5, send/1]).
-export([get_timestamp/0]).
-export([pause_calculation/1, restart_calculation/4, restart_calculation/1, restart_calculation/3]).
-export([restart_measurement/2, pause_measurement/1]).
-export([restart_sync_measurement/3, restart_sync_measurement/5]).
-export([restart_unsync_measurement/4, restart_unsync_measurement/6]).
-export([get_calculation/4, get_synchronized_measurement/5, get_unsynchronized_measurement/6]).
-export([maybe_propagate/1]).
-export([test_launch_hera/0]).

% Callbacks
-export([start/2]).
-export([stop/1]).


%--- Callbacks -----------------------------------------------------------------

%% @private
start(_Type, _Args) ->
  %{ok, _} = application:ensure_all_started(hera),
  %application:start(kernel),
  %application:start(stdlib),
  %%hera_pool:start_link(). % verif bon appel?
  hera_supersup2:start_link().

%% @private
stop(_State) -> ok.

%% ===================================================================
%% API
%% ===================================================================

%% -------------------------------------------------------------------
%% @doc
%% Start all pools. Function to be called by GRiSP boards
%%
%% @param Measurements List of measurements to perform on the node
%% @param Calculations List of calculations to perform on the node
%% @param Master set to true if the current node must start the global_sync server, false otherwise
%%
%% @spec launch_app(
%%            Measurements :: list(unsync_measurement() | sync_measurement()),
%%            Calculations :: list(calculation()),
%%            Master :: boolean()
%%       ) -> ok
%% @end
%% -------------------------------------------------------------------
-spec launch_app(Measurements :: list(unsync_measurement() | sync_measurement()), Calculations :: list(calculation()), Master :: boolean()) -> ok.
launch_app(Measurements, Calculations, Master) ->

  %% starts hera_sensors_data
  hera_pool:start_pool(sensor_data_pool, 1, {hera_sensors_data, start_link, []}),
  hera_pool:run(sensor_data_pool, []),

  %% starts hera_communications
  hera_pool:start_pool(communicationsPool, 1, {hera_communications, start_link, []}),
  hera_pool:run(communicationsPool, []),

  %% starts hera_multicast
  hera_pool:start_pool(multicastPool, 1, {hera_multicast, start_link, []}),
  hera_pool:run(multicastPool, []),

  %% if this node is the master node, starts the global_sync module
  case Master of
    true ->
      hera_pool:start_pool(hera_global_pool, 1, {hera_global_sync, start_link, []}),
      hera_pool:run(hera_global_pool, []),
      SyncMeas = lists:filter(fun({_Name, Meas}) -> maps:get(synchronization, Meas) end, Measurements),
      hera_pool:start_pool(global_dispatch_pool, length(SyncMeas), {hera_global_dispatch, start_link, []}),
      [hera_pool:run(global_dispatch_pool, [Name, hera_utils:concat_atoms(dispatch_, Name)]) || {Name, _M} <- SyncMeas];
    false -> not_master
  end,

  %% starts hera_synchronization
  hera_pool:start_pool(hera_synchronization_pool, 1, {hera_synchronization, start_link, []}),
  hera_pool:run(hera_synchronization_pool, []),

  %% starts hera_filter
  hera_pool:start_pool(filter_data_pool, 1, {hera_filter, start_link, []}),
  hera_pool:run(filter_data_pool, []),

  %% starts hera_measure
  hera_pool:start_pool(measurement_pool, length(Measurements), {hera_measure, start_link, []}),
  MeasurementsPids = [{Name, hera_pool:run(measurement_pool, [{Name, Measurement}])} || {Name, Measurement} <- Measurements],
  [register(Name, Pid) || {Name, {ok, Pid}} <- MeasurementsPids],

  %% start hera_calculation
  hera_pool:start_pool(calculation_pool, length(Calculations), {hera_calculation, start_link, []}),
  CalculationsPids = [{Name, hera_pool:run(calculation_pool, [Name, maps:get(func, Calculation), maps:get(frequency, Calculation), maps:get(max_iterations, Calculation)])} || {Name, Calculation} <- Calculations],
  [register(Name, Pid) || {Name, {ok, Pid}} <- CalculationsPids],
  started.

%% -------------------------------------------------------------------
%% @doc
%% Start only multicast pool. Function to be called by a shell on a computer
%%
%% @spec launch_app() -> ok
%% @end
%% -------------------------------------------------------------------
-spec launch_app() -> ok.
launch_app() ->
  %% starts hera_sensors_data
  hera_pool:start_pool(sensor_data_pool, 1, {hera_sensors_data, start_link, []}),
  hera_pool:run(sensor_data_pool, []),

  %% starts hera_communications
  hera_pool:start_pool(communicationsPool, 1, {hera_communications, start_link, []}),
  hera_pool:run(communicationsPool, []),

  %% starts hera_multicast
  hera_pool:start_pool(multicastPool, 1, {hera_multicast, start_link, []}),
  hera_pool:run(multicastPool, []),
  started.

test_launch_hera() ->
  Measurements = [hera:get_unsynchronized_measurement(test1, fun() -> {ok, 10.0} end, false, 0.17, 10, 2000), hera:get_unsynchronized_measurement(test2, fun() -> {ok, 20.0} end, false, 0.17, 10, 2000)],
  hera_pool:start_pool(measurement_pool, length(Measurements), {hera_measure, start_link, []}),
  MeasurementsPids = [{Name, hera_pool:run(measurement_pool, [{Name, Measurement}])} || {Name, Measurement} <- Measurements],
  [register(Name, Pid) || {Name, {ok, Pid}} <- MeasurementsPids],
  started.

%% -------------------------------------------------------------------
%% @doc
%% Send a data over the multicast cluster
%%
%% @param MessageType The type of the message to be sent, either calc or measure
%% @param Name The name of the sent data
%% @param Node The node which send the message
%% @param Seqnum The sequence number of the data
%% @param Data The data to be sent
%%
%% @spec send(MessageType :: calc | measure, Name :: atom(), Node :: atom(), Seqnum :: integer(), Data :: term()) -> ok
%% @end
%% -------------------------------------------------------------------
-spec send(MessageType :: calc | measure, Name :: atom(), Node :: atom(), Seqnum :: integer(), Data :: term()) -> ok.
send(MessageType, Name, Node, Seqnum, Data) ->
  hera_multicast:send(MessageType, Name, Node, Seqnum, Data).

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
  hera_multicast:send(Message).

%%--------------------------------------------------------------------
%% @doc
%% Restart worker that performs the calculation <Name> from zero with a new function
%%
%% @param Name The name of the measurement
%% @param Func The calculation function to be executed
%% @param Args The arguments of the function
%% @param Frequency The frequency of the calculation
%% @param MaxIterations The number of iterations to be done
%%
%% @spec restart_calculation(Name :: atom(), Func :: fun((...) -> {ok, term()} | {error, term()}), Args :: list(any()), Frequency :: integer(), MaxIterations :: integer()) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec restart_calculation(Name :: atom(), Func :: fun((...) -> {ok, term()} | {error, term()}), Frequency :: integer(), MaxIterations :: integer()) -> ok.
restart_calculation(Name, Func, Frequency, MaxIterations) ->
  hera_calculation:restart_calculation(Name, Func, Frequency, MaxIterations),
  ok.

%%--------------------------------------------------------------------
%% @doc
%% Restart worker that performs the calculation <Name> from zero if the previous calculation has terminated, or from the previous state if it is pause.
%%
%% @param Name The name of the calculation
%%
%% @spec restart_calculation(Name :: atom()) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec restart_calculation(Name :: atom()) -> ok.
restart_calculation(Name) ->
  hera_calculation:restart_calculation(Name),
  ok.

%%--------------------------------------------------------------------
%% @doc
%% Restart worker that performs the calculation <Name> from zero with new frequency and number of iterations
%%
%% @param Name The name of the calculation
%% @param Frequency The frequency of the calculation
%% @param MaxIterations The number of iterations to be done
%%
%% @spec restart_calculation(Name :: atom(), Frequency :: integer(), MaxIterations :: integer() | infinity) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec restart_calculation(Name :: atom(), Frequency :: integer(), MaxIterations :: integer() | infinity) -> ok.
restart_calculation(Name, Frequency, MaxIterations) ->
  hera_calculation:restart_calculation(Name, Frequency, MaxIterations),
  ok.

%%--------------------------------------------------------------------
%% @doc
%% Pause the worker that performs the calculation <Name>
%%
%% @param Name The name of the calculation
%%
%% @spec pause_calculation(Name :: atom()) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec pause_calculation(Name :: atom()) -> ok.
pause_calculation(Name) ->
  hera_calculation:pause_calculation(Name),
  ok.

%%--------------------------------------------------------------------
%% @doc
%% Restart worker that performs the unsynchronized measurement <Name> from zero with new parameters
%%
%% @param Name The name of the measurement
%% @param Func The measurement function to be executed
%% @param Frequency The frequency of the measurement
%% @param MaxIterations The number of iterations to be done
%% @param Filtering Boolean that indicates if a filtering must be done to the data output by the function
%% @param WarmUpPhase Boolean that indicates if a warm-up phase must be performed
%%
%% @spec restart_unsync_measurement(Name :: atom(), Func :: fun((...) -> {ok, term()} | {error, term()}), Args :: list(any()), Frequency :: integer(), MaxIterations :: integer(), Filtering :: boolean()) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec restart_unsync_measurement(Name :: atom(), Func ::fun((...) -> {ok, term()} | {error, term()}), Frequency :: integer(), MaxIterations :: integer(), Filtering :: boolean(), WarmUpPhase :: boolean()) -> ok.
restart_unsync_measurement(Name, Func, Frequency, MaxIterations, Filtering, WarmUpPhase) ->
  hera_measure:restart_unsync_measurement(Name, Func, Frequency, MaxIterations, Filtering, WarmUpPhase).

%%--------------------------------------------------------------------
%% @doc
%% Restart worker that performs the synchronized measurement <Name> from zero with new parameters
%%
%% @param Name The name of the measurement
%% @param Func The measurement function to be executed
%% @param MaxIterations The number of iterations to be done
%% @param Filtering Boolean that indicates if a filtering must be done to the data output by the function
%% @param WarmUpPhase Boolean that indicates if a warm-up phase must be performed
%%
%% @spec restart_unsync_measurement(Name :: atom(), Func :: fun((...) -> {ok, term()} | {error, term()}), Args :: list(any()), Frequency :: integer(), MaxIterations :: integer(), Filtering :: boolean()) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec restart_sync_measurement(Name :: atom(), Func ::fun((...) -> {ok, term()} | {error, term()}), MaxIterations :: integer(), Filtering :: boolean(), WarmUpPhase :: boolean()) -> ok.
restart_sync_measurement(Name, Func, MaxIterations, Filtering, WarmUpPhase) ->
  hera_measure:restart_sync_measurement(Name, Func, MaxIterations, Filtering, WarmUpPhase).

%%--------------------------------------------------------------------
%% @doc
%% Restart worker that performs the unsynchronized or synchronized measurement <Name> from zero if the previous calculation has terminated, or from the previous state if it is pause.
%%
%% @param Name The name of the measurement
%% @param WarmUpPhase Boolean that indicates if a warm-up phase must be performed
%%
%% @spec restart_measurement(Name :: atom()) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec restart_measurement(Name :: atom(), WarmUpPhase :: boolean()) -> ok.
restart_measurement(Name, WarmUpPhase) ->
  hera_measure:restart_measurement(Name, WarmUpPhase).

%%--------------------------------------------------------------------
%% @doc
%% Restart worker that performs the synchronized measurement <Name> from zero if the previous calculation has terminated, or from the previous state if it is pause.
%%
%% @param Name The name of the measurement
%% @param MaxIterations The number of iterations to be done
%% @param WarmUpPhase Boolean that indicates if a warm-up phase must be performed
%%
%% @spec restart_sync_measurement(Name :: atom()) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec restart_sync_measurement(Name :: atom(), MaxIteration :: integer(), WarmUpPhase :: boolean()) -> ok.
restart_sync_measurement(Name, MaxIterations, WarmUpPhase) ->
  hera_measure:restart_sync_measurement(Name, MaxIterations, WarmUpPhase).


%%--------------------------------------------------------------------
%% @doc
%% Restart worker that performs the unsynchronized measurement <Name> from zero with new frequency and number of iterations
%%
%% @param Name The name of the measurement
%% @param Frequency The frequency of the measurement
%% @param MaxIterations The number of iterations to be done
%% @param WarmUpPhase Boolean that indicates if a warm-up phase must be performed
%%
%% @spec restart_unsync_measurement(Name :: atom(), Frequency :: integer(), MaxIterations :: integer() | infinity) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec restart_unsync_measurement(Name :: atom(), Frequency :: integer(), MaxIterations :: integer(), WarmUpPhase :: boolean()) -> ok.
restart_unsync_measurement(Name, Frequency, MaxIterations, WarmUpPhase) ->
  hera_measure:restart_unsync_measurement(Name, Frequency, MaxIterations, WarmUpPhase),
  ok.

%%--------------------------------------------------------------------
%% @doc
%% Pause the worker that performs the synchronized or unsynchronized measurement <Name>
%%
%% @param Name The name of the measurement
%%
%% @spec pause_measurement(Name :: atom()) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec pause_measurement(Name :: atom()) -> ok.
pause_measurement(Name) ->
  hera_measure:pause_measurement(Name),
  ok.

%%--------------------------------------------------------------------
%% @doc
%% Return a well formed synchronized measurement
%%
%% @param Name The name of the measurement
%% @param Func The function that performs the measurement
%% @param Filtering A boolean that indicates if the resulting measures must be filtered
%% @param UpperBound TODO
%% @param MaxIterations The number of measurements to be performed
%%
%% @spec get_synchronized_measurement(Name :: atom()
%%                                    , Func :: fun(() -> {ok, term()} | {error, term()} )
%%                                    , Filtering :: boolean()
%%                                    , UpperBound :: float()
%%                                    , MaxIterations :: integer() | infinity)
%%      -> sync_measurement().
%% @end
%%--------------------------------------------------------------------
-spec get_synchronized_measurement(Name :: atom(), Func :: fun(() -> {ok, term()} | {error, term()} ), Filtering :: boolean(), UpperBound :: float(), MaxIterations :: integer() | infinity) -> sync_measurement().
get_synchronized_measurement(Name, Func, Filtering, UpperBound, MaxIterations) ->
  {Name, #{func => Func, filtering => Filtering, upper_bound => UpperBound, max_iterations => MaxIterations, synchronization => true}}.

%%--------------------------------------------------------------------
%% @doc
%% Return a well formed unsynchronized measurement
%%
%% @param Name The name of the measurement
%% @param Func The function that performs the measurement
%% @param Filtering A boolean that indicates if the resulting measures must be filtered
%% @param UpperBound TODO
%% @param MaxIterations The number of measurements to be performed
%% @param Frequency The frequency at which the measurements are done
%%
%% @spec get_unsynchronized_measurement(Name :: atom()
%%                                      , Func :: fun(() -> {ok, term()} | {error, term()} )
%%                                      , Filtering :: boolean()
%%                                      , UpperBound :: float()
%%                                      , MaxIteration :: integer() | infinity
%%                                      , Frequency :: integer())
%%      -> unsync_measurement().
%% @end
%%--------------------------------------------------------------------
-spec get_unsynchronized_measurement(Name :: atom(), Func :: fun(() -> {ok, term()} | {error, term()} ), Filtering :: boolean(), UpperBound :: float(), MaxIteration :: integer() | infinity, Frequency :: integer()) -> unsync_measurement().
get_unsynchronized_measurement(Name, Func, Filtering, UpperBound, MaxIteration, Frequency) ->
  {Name, #{func => Func, filtering => Filtering, upper_bound => UpperBound, max_iterations => MaxIteration, frequency => Frequency, synchronization => false}}.

%%--------------------------------------------------------------------
%% @doc
%% Return a well formed calculation
%%
%% @param Name The name of the calculation
%% @param Func The function that performs the calculation
%% @param MaxIterations The number of calculations to be performed
%% @param Frequency The frequency at which the calculations are done
%%
%% @spec get_calculation(Name :: atom
%%                       , Func :: fun(() -> {ok, term()} | {error, term()})
%%                       , Frequency :: integer()
%%                       , MaxIterations :: integer() | infinity)
%%      -> calculation().
%% @end
%%--------------------------------------------------------------------
-spec get_calculation(Name :: atom, Func :: fun(() -> {ok, term()} | {error, term()}), Frequency :: integer(), MaxIterations :: integer() | infinity) -> calculation().
get_calculation(Name, Func, Frequency, MaxIterations) ->
  {Name, #{func => Func, frequency => Frequency, max_iterations => MaxIterations}}.

%%--------------------------------------------------------------------
%% @doc
%% Propagate a function to all node. When receiving the function, the node executes it.
%%
%% @param Fun The function to be propagated
%%
%% @spec maybe_propagate(Fun :: function()) -> any().
%% @end
%%--------------------------------------------------------------------
-spec maybe_propagate(Fun :: function()) -> any().
maybe_propagate(Fun) ->
  hera:send({propagate, Fun}),
  catch Fun().


%% @private
-spec get_timestamp() -> integer().
get_timestamp() ->
  erlang:monotonic_time(millisecond).
  %{Mega, Sec, Micro} = os:timestamp(),
  %(Mega*1000000 + Sec)*1000 + round(Micro/1000).

%% @private
fake_sonar_get() ->
  float(rand:uniform(10)).
