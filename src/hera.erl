% @doc hera public API.
% @end
-module(hera).
-author("Julien Bastin <julien.bastin@student.uclouvain.be>, Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>").

-behavior(application).

-include("hera.hrl").

%% API/home/julien/home/julien/home/julien
-export([launch_app/0]).
-export([clusterize/0]).
-export([fake_sonar_get/0]).
-export([send/1]).
-export([store_data/3]).
-export([get_data/0]).
-export([perform_measures/5]).
-export([get_timestamp/0]).

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
%% Start all pools
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

%-spec launch_app(Measurement_func :: function(), Measurement_frequency :: integer(), Calculation_function :: function(), Calculation_frequency :: integer()) -> ok.
%launch_app(Measurement_func, Measurement_frequency, Calculation_function, Calculation_frequency) ->
-spec launch_app() -> ok.
launch_app()->
  hera_pool:start_pool(sensor_data_pool, 1, {hera_sensors_data, start_link, []}),
  hera_pool:run(sensor_data_pool, []),
  hera_pool:start_pool(multicastPool, 1, {hera_multicast, start_link, []}),
  hera_pool:run(multicastPool, []),


  hera_pool:start_pool(filter_data_pool, 1, {hera_filter, start_link, []}),
  hera_pool:run(filter_data_pool, []),
  hera_pool:start_pool(pool1, 1, {hera_measure, start_link, []}),
  %hera_pool:run(pool1, [Measurement_func, Measurement_frequency]),
  hera_pool:run(pool1, []),
  hera_pool:start_pool(pool2, 1, {hera_position, start_link, []}),
  %hera_pool:run(pool2, [Calculation_function, Calculation_frequency]),
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
%% Perform measures
%%
%% @param Delay frequency of measurements
%% @param Max_iter number of measures to perform
%% @param File_name name of the file storing the data
%% @param Func the function to perform the measure
%%
%% @spec perform_measures(Delay :: integer(), Max_iter :: integer(), File_name :: file:name_all(), Func :: function()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec perform_measures(Max_iter :: integer(), Delay :: integer(), Measure_func :: function(), Do_filter :: boolean(), Do_sonar_warmup :: boolean()) -> ok.
perform_measures(Max_iter, Delay, Measure_func, Do_filter, Do_sonar_warmup) ->
  hera_measure:perform_measures(Max_iter, Delay, Measure_func, Do_filter, Do_sonar_warmup).



-spec get_timestamp() -> integer().
get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).




fake_sonar_get() ->
  float(rand:uniform(10)).
