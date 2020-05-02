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
-export([launch_app/2]).
-export([launch_app/0]).
-export([clusterize/0]).
-export([fake_sonar_get/0]).
-export([send/1]).
-export([store_data/4]).
-export([get_data/1]).
-export([log_measure/4]).
-export([log_calculation/4]).

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
%% @param Measurements
%% @param Calculations
%%
%% @spec launch_app(
%%            Measurements :: list(measurement()),
%%            Calculations :: list(calculation())
%%       ) -> ok
%% @end
%% -------------------------------------------------------------------
-spec launch_app(Measurements :: list(measurement()), Calculations :: list(calculation())) -> ok.
launch_app(Measurements, Calculations) ->
  hera_pool:start_pool(sensor_data_pool, 1, {hera_sensors_data, start_link, []}),
  hera_pool:run(sensor_data_pool, []),
  hera_pool:start_pool(multicastPool, 1, {hera_multicast, start_link, []}),
  hera_pool:run(multicastPool, []),
  hera_pool:start_pool(measurement_pool, length(Measurements), {hera_measure, start_link, []}),
  [hera_pool:run(measurement_pool, [Name, maps:get(func, Measurement), maps:get(args, Measurement), maps:get(frequency, Measurement)]) || {Name, Measurement} <- Measurements],
  hera_pool:start_pool(calculation_pool, length(Calculations), {hera_calculation, start_link, []}),
  [hera_pool:run(calculation_pool, [Name, maps:get(func, Calculation), maps:get(args, Calculation), maps:get(frequency, Calculation)]) || {Name, Calculation} <- Calculations],
  clusterize().

%% -------------------------------------------------------------------
%% @doc
%% Start only multicast pool. Function to be called by a shell on a computer
%%
%% @spec launch_app() -> ok
%% @end
%% -------------------------------------------------------------------
-spec launch_app() -> ok.
launch_app() ->
  hera_pool:start_pool(multicastPool, 1, {hera_multicast, start_link, []}),
  hera_pool:run(multicastPool, []),
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
%% Send a message over the multicast cluster
%%
%% @param Message the message to be send
%%
%% @spec send(Message :: term()) -> ok
%% @end
%% -------------------------------------------------------------------
-spec send(Message :: term()) -> ok.
send(Message) ->
  hera_multicast:send(term_to_binary(Message)).

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
-spec get_data(Name :: atom()) -> dict:dict(string(), {integer(), integer() | float()}).
get_data(Name) ->
  hera_sensors_data:get_data(Name).

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

%% @private
fake_sonar_get() ->
  float(rand:uniform(10)).
