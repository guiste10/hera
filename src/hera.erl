% @doc hera public API.
% @end
-module(hera).
-author("Julien Bastin <julien.bastin@student.uclouvain.be>, Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>").

-behavior(application).

-include("hera.hrl").

%% API/home/julien/home/julien/home/julien
-export([launch_app/3]).
-export([clusterize/0]).
-export([fake_sonar_get/0]).
-export([send/1]).
-export([store_data/3]).
-export([get_data/0]).

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
launch_app(Delay, Max_iter, File_name) ->
  hera_pool:start_pool(pool1, 1, {single_sonar_test, start_link, []}),
  hera_pool:run(pool1, [Delay, Max_iter, File_name]).

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

fake_sonar_get() ->
  rand:uniform(10).