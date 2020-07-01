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
-export([clusterize/0]).
-export([fake_sonar_get/0]).
-export([send/5, send/1]).
-export([store_data/4]).
-export([get_data/1]).
-export([get_recent_data/1]).
-export([log_measure/4]).
-export([log_calculation/4]).
-export([get_timestamp/0]).
-export([pause_calculation/1, restart_calculation/4, restart_calculation/1, restart_calculation/3]).
-export([restart_measurement/5, restart_measurement/1, restart_measurement/3, pause_measurement/1]).
-export([get_calculation/4, get_synchronized_measurement/5, get_unsynchronized_measurement/6]).

% Callbacks
-export([start/2]).
-export([stop/1]).


%--- Callbacks -----------------------------------------------------------------

%% @private
start(_Type, _Args) ->
  %{ok, _} = application:ensure_all_started(hera),
  %application:start(kernel),
  %application:start(stdlib),
  hera_pool:start_link(). % verif bon appel?

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
  %% starts multicast
  clusterize(),

  %% if this node is the master node, starts the global_sync module
  case Master of
    true ->
      hera_pool:start_pool(hera_global_pool, 1, {hera_global_sync, start_link, []}),
      {ok, GPid} = hera_pool:run(hera_global_pool, []),
      global:register_name(?SYNC_PROC, GPid);
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
  [register(Name, Pid) || {Name, {ok, Pid}} <- CalculationsPids].

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
  %% starts multicast
  clusterize().

%% -------------------------------------------------------------------
%% @doc
%% Start the formation of an udp multicast cluster
%%
%% @spec clusterize() -> ok
%% @end
%% -------------------------------------------------------------------
-spec clusterize() -> ok.
clusterize() ->
  hera_multicast:formation().

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

%% -------------------------------------------------------------------
%% @doc
%% Add a new data for the specified node
%%
%% @param Name The name/type of the data (e.g. temperature, sonar, humidity, ...)
%% @param Node The node who perform the measurement of the data
%% @param Seqnum The sequence number of the measured data
%% @param Data The data measured by the sensor
%%
%% @spec store_data(Name :: atom(), Node :: atom(), Seqnum :: integer(), Data :: integer() | float()) -> ok
%% @end
%% -------------------------------------------------------------------
-spec store_data(Name :: atom(), Node :: atom(), Seqnum :: integer(), Data :: integer() | float()) -> ok.
store_data(Name, Node, Seqnum, Data) ->
  hera_sensors_data:store_data(Name, Node, Seqnum, Data).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve the data of sensors of all nodes
%%
%% @spec get_data() -> dict:dict(string(), {integer(), integer() | float()})
%% @end
%%--------------------------------------------------------------------
-spec get_data(Name :: atom()) -> dict:dict(string(), {integer(), integer() | float(), integer()}).
get_data(Name) ->
  hera_sensors_data:get_data(Name).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve the recent (+-500ms or less) data of the sensors of all nodes
%%
%% @spec get_data() -> dict:dict(string(), {integer(), integer() | float()})
%% @end
%%--------------------------------------------------------------------
-spec get_recent_data(Name :: atom()) -> dict:dict(string(), {integer(), integer() | float(), integer()}).
get_recent_data(Name) ->
  hera_sensors_data:get_recent_data(Name).

%%--------------------------------------------------------------------
%% @doc
%% Log the given measure into a file with the same name as the node name
%%
%% @param Name The name/type of the measure (e.g. temperature, sonar, humidity, ...)
%% @param Node The node who perform the measurement of the data
%% @param Seqnum The sequence number of the measured data
%% @param Data The data measured by the sensor
%%
%% @spec log_measure(Name :: atom(), Node :: atom(), Seqnum :: integer(), Data :: integer() | float()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec log_measure(Name :: atom(), Node :: atom(), Seqnum :: integer(), Data :: integer() | float()) -> ok.
log_measure(Name, Node, Seqnum, Data) ->
  hera_sensors_data:log_measure(Name, Node, Seqnum, Data).

%%--------------------------------------------------------------------
%% @doc
%% Log the given measure into a file with the same name as the node name
%%
%% @param Name The name of the calculation (e.g. position_calculation, temperature_median, ...)
%% @param Node The node who perform the calculation
%% @param Seqnum The sequence number of the calculation result
%% @param Result The result of the calculation
%%
%% @spec log_calculation(Name :: atom(), Node :: atom(), Seqnum :: integer(), Data :: integer() | float()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec log_calculation(Name :: atom(), Node :: atom(), Seqnum :: integer(), Result :: integer() | float()) -> ok.
log_calculation(Name, Node, Seqnum, Result) ->
  hera_sensors_data:log_calculation(Name, Node, Seqnum, Result).

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
%% Restart worker that performs the measurement <Name> from zero with a new function
%%
%% @param Name The name of the measurement
%% @param Func The measurement function to be executed
%% @param Frequency The frequency of the measurement
%% @param MaxIterations The number of iterations to be done
%% @param Filtering Boolean that indicates if a filtering must be done to the data output by the function
%%
%% @spec restart_measurement(Name :: atom(), Func :: fun((...) -> {ok, term()} | {error, term()}), Args :: list(any()), Frequency :: integer(), MaxIterations :: integer(), Filtering :: boolean()) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec restart_measurement(Name :: atom(), Func ::fun((...) -> {ok, term()} | {error, term()}), Frequency :: integer(), MaxIterations :: integer(), Filtering :: boolean()) -> ok.
restart_measurement(Name, Func, Frequency, MaxIterations, Filtering) ->
  hera_measure:restart_measurement(Name, Func, Frequency, MaxIterations, Filtering),
  ok.

%%--------------------------------------------------------------------
%% @doc
%% Restart worker that performs the measurement <Name> from zero if the previous calculation has terminated, or from the previous state if it is pause.
%%
%% @param Name The name of the measurement
%%
%% @spec restart_measurement(Name :: atom()) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec restart_measurement(Name :: atom()) -> ok.
restart_measurement(Name) ->
  hera_measure:restart_measurement(Name),
  ok.

%%--------------------------------------------------------------------
%% @doc
%% Restart worker that performs the measurement <Name> from zero with new frequency and number of iterations
%%
%% @param Name The name of the measurement
%% @param Frequency The frequency of the measurement
%% @param MaxIterations The number of iterations to be done
%%
%% @spec restart_measurement(Name :: atom(), Frequency :: integer(), MaxIterations :: integer() | infinity) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec restart_measurement(Name :: atom(), Frequency :: integer(), MaxIterations :: integer()) -> ok.
restart_measurement(Name, Frequency, MaxIterations) ->
  hera_measure:restart_measurement(Name, Frequency, MaxIterations),
  ok.

%%--------------------------------------------------------------------
%% @doc
%% Pause the worker that performs the measurement <Name>
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


%% @private
-spec get_timestamp() -> integer().
get_timestamp() ->
  erlang:monotonic_time(millisecond).
  %{Mega, Sec, Micro} = os:timestamp(),
  %(Mega*1000000 + Sec)*1000 + round(Micro/1000).

%% @private
fake_sonar_get() ->
  float(rand:uniform(10)).
