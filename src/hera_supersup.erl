%%%-------------------------------------------------------------------
%%% @author julien
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2020 4:58 PM
%%%-------------------------------------------------------------------
-module(hera_supersup).
-author("julien").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link(OsType :: {unix | win32, atom()}) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(OsType) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, OsType).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]}}
  | ignore | {error, Reason :: term()}).
init(OsType) when OsType == {unix, rtems} ->
  MaxRestarts = 6,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = #{strategy => rest_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

  SensorsData = #{id => hera_sensors_data,
    start => {hera_sensors_data, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker},

  SupervisorCalculation = #{id => hera_sup_calculation,
    start => {hera_sup, start_link, [calculation_pool, 1, {hera_calculation, start_link, []}]},
    restart => permanent,
    shutdown => 2000,
    type => supervisor},

  SupervisorMeasures = #{id => hera_sup_measure,
    start => {hera_sup, start_link, [measurement_pool, 1, {hera_measure, start_link, []}]},
    restart => permanent,
    shutdown => 2000,
    type => supervisor},

  SupervisorMeasFilters = #{
    id => hera_sup_meas_filter,
    start => {hera_serv, start_link, [meas_filter_pool, 1, supervisor_measurement, {hera_filter, start_link, []}, meas]},
    restart => permanent,
    shutdown => 2000,
    type => supervisor
  },

  SupervisorCalcFilters = #{
    id => hera_sup_calc_filter,
    start => {hera_serv, start_link, [calc_filter_pool, 1, supervisor_calculation, {hera_filter, start_link, []}, calc]},
    restart => permanent,
    shutdown => 2000,
    type => supervisor
  },

  SupervisorMeasurements = #{id => hera_sup_measurement,
    start => {hera_sup2, start_link, [
      supervisor_measurement,
      rest_for_one, [
        #{id => hera_synchronization,
          start => {hera_synchronization, start_link, []}},
        %%SupervisorMeasFilters,
        SupervisorMeasures
      ]
    ]},
    restart => permanent,
    shutdown => 2000,
    type => supervisor},

  SupervisorCalculations = #{id => hera_sup_calculations,
    start => {hera_sup2, start_link, [
      supervisor_calculation,
      rest_for_one, [
        %%SupervisorCalcFilters,
        SupervisorCalculation
      ]
    ]},
    restart => permanent,
    shutdown => 2000,
    type => supervisor},

  SupervisorCalcMeas = #{id => hera_sup_calc_meas,
    start => {hera_sup2, start_link, [
      supervisor_calc_meas,
      one_for_one, [
        SupervisorMeasurements, SupervisorCalculations
      ]
    ]},
    restart => permanent,
    shutdown => 2000,
    type => supervisor},

  Supervisor1 = #{id => hera_sup1,
    start => {hera_sup2, start_link, [
      supervisor_1,
      rest_for_one, [
        #{id => hera_communications,
          start => {hera_communications, start_link, []}},
        #{id => hera_multicast,
          start => {hera_multicast, start_link, []}},
        SupervisorCalcMeas
      ]
    ]},
    restart => permanent,
    shutdown => 2000,
    type => supervisor},

  {ok, {SupFlags, [SensorsData, Supervisor1]}};

init(_OsType) ->
  MaxRestarts = 6,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = #{strategy => rest_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

  SensorsData = #{id => hera_sensors_data,
    start => {hera_sensors_data, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker},

  Supervisor1 = #{id => hera_sup1,
    start => {hera_sup2, start_link, [
      supervisor_1,
      rest_for_one, [
        #{id => hera_communications,
          start => {hera_communications, start_link, []}},
        #{id => hera_multicast,
          start => {hera_multicast, start_link, []}}
      ]
    ]},
    restart => permanent,
    shutdown => 2000,
    type => supervisor},

  {ok, {SupFlags, [SensorsData, Supervisor1]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
