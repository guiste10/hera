% @doc hera public API.
% @end
-module(hera).
-author("Julien Bastin <julien.bastin@student.uclouvain.be>, Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>").

-behavior(application).

-include("hera.hrl").

%% API
-export([launch_app/3]).
-export([launch_app/2]).
-export([clusterize/0]).
-export([fake_sonar_get/0]).
-export([send/1]).
-export([store_data/3]).
-export([get_data/0]).
-export([log_measure/3]).

% Callbacks
-export([start/2]).
-export([stop/1]).


%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) ->
  %{ok, _} = application:ensure_all_started(hera),
  %application:start(kernel),
  %application:start(stdlib),
  hera_pool:start_link(). % verif bon appel?

stop(_State) -> ok.

%% ===================================================================
%% API
%% ===================================================================

%% -------------------------------------------------------------------
%% @doc
%% Start all pools. Function to be called by GRiSP boards
%%
%% @param Measurement_func function to get measures
%% @param Measurement_frequency frequency at which measurements are taken (in ms)
%% @param Calculation_function function that use the measurements to compute a result
%% @param Calculation_frequency frequency at which the calculation is perform (in ms)
%%
%% @spec launch_app(
%%            Measurement_func :: function(),
%%            Measurement_frequency :: integer(),
%%            Calculation_function :: function(),
%%            Calculation_frequency :: integer())
%%        -> ok
%% @end
%% -------------------------------------------------------------------
-spec launch_app(Measurement_func :: function(), Measurement_frequency :: integer(), Calculations :: list(calculation())) -> ok.
launch_app(Measurement_func, Measurement_frequency, Calculations) ->
  hera_pool:start_pool(sensor_data_pool, 1, {hera_sensors_data, start_link, []}),
  hera_pool:run(sensor_data_pool, []),
  hera_pool:start_pool(multicastPool, 1, {hera_multicast, start_link, []}),
  hera_pool:run(multicastPool, []),
  hera_pool:start_pool(measurement_pool, 1, {hera_measure, start_link, []}),
  hera_pool:run(measurement_pool, [Measurement_func, Measurement_frequency]),
  hera_pool:start_pool(calculation_pool, length(Calculations), {hera_calculation, start_link, []}),
  [hera_pool:run(calculation_pool, [Name, maps:get(func, Calculation), maps:get(args, Calculation), maps:get(frequency, Calculation)]) || {Name, Calculation} <- Calculations],
  clusterize().

%% -------------------------------------------------------------------
%% @doc
%% Start all pools. Function to be called by a shell on a computer
%%
%% @param Measurement_func function to get measures
%% @param Measurement_frequency frequency at which measurements are taken (in ms)
%% @param Calculation_function function that use the measurements to compute a result
%% @param Calculation_frequency frequency at which the calculation is perform (in ms)
%%
%% @spec launch_app(
%%            Calculation_function :: function(),
%%            Calculation_frequency :: integer())
%%        -> ok
%% @end
%% -------------------------------------------------------------------
-spec launch_app(Calculation_function :: function(), Calculation_frequency :: integer()) -> ok.
launch_app(Calculation_function, Calculation_frequency) ->
  hera_pool:start_pool(sensor_data_pool, 1, {hera_sensors_data, start_link, []}),
  hera_pool:run(sensor_data_pool, []),
  hera_pool:start_pool(multicastPool, 1, {hera_multicast, start_link, []}),
  hera_pool:run(multicastPool, []),
  hera_pool:start_pool(pool2, 1, {hera_position, start_link, []}),
  hera_pool:run(pool2, [Calculation_function, Calculation_frequency]),
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
%% @spec send(Message :: binary()) -> ok
%% @end
%% -------------------------------------------------------------------
-spec send(Message :: binary()) -> ok.
send(Message) ->
  hera_multicast:send(Message).

%% -------------------------------------------------------------------
%% @doc
%% Add a new data for the specified node
%%
%% @spec store_data(Node :: string(), Seqnum :: integer(), Data :: integer() | float()) -> ok
%% @end
%% -------------------------------------------------------------------
-spec store_data(Node :: string(), Seqnum :: integer(), Data :: integer() | float()) -> ok.
store_data(Node, Seqnum, Data) ->
  hera_sensors_data:store_data(Node, Seqnum, Data).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve the data of sensors of all nodes
%%
%% @spec get_data() -> dict:dict(string(), {integer(), integer() | float()})
%% @end
%%--------------------------------------------------------------------
-spec get_data() -> dict:dict(string(), {integer(), integer() | float()}).
get_data() ->
  hera_sensors_data:get_data().

%%--------------------------------------------------------------------
%% @doc
%% Log the given measure into a file with the same name as the node name
%%
%% @spec store_data(Node :: string(), Seqnum :: integer(), Data :: integer() | float()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec log_measure(Node :: string(), Seqnum :: integer(), Data :: integer() | float()) -> ok.
log_measure(Node, Seqnum, Data) ->
  hera_sensors_data:log_measure(Node, Seqnum, Data).

fake_sonar_get() ->
  float(rand:uniform(10)).
