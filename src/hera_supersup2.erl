%%%-------------------------------------------------------------------
%%% @author julien
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2020 4:58 PM
%%%-------------------------------------------------------------------
-module(hera_supersup2).
-author("julien").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
init([]) ->
  MaxRestarts = 6,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = #{strategy => rest_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

  SensorsData = #{id => hera_sensors_data,
    start => {hera_sensors_data, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [hera_sensors_data]},

  SupervisorCalculation = #{id => hera_sup_calculation,
    start => {hera_sup2, start_link, [
      supervisor_calculation,
      simple_one_for_one, []
    ]},
    restart => permanent,
    shutdown => 2000,
    type => supervisor,
    modules => [hera_sup2]},

  SupervisorMeasures = #{id => hera_sup_measure,
    start => {hera_sup2, start_link, [
      supervisor_measures,
      simple_one_for_one, []
    ]},
    restart => permanent,
    shutdown => 2000,
    type => supervisor,
    modules => [hera_sup2]},

  SupervisorMeasurements = #{id => hera_sup_measurement,
    start => {hera_sup2, start_link, [
      supervisor_measurement,
      rest_for_one, [
        #{id => hera_synchronization,
          start => {hera_synchronization, start_link, []}},
        #{id => hera_filter,
          start => {hera_filter, start_link, []}},
        SupervisorMeasures
      ]
    ]},
    restart => permanent,
    shutdown => 2000,
    type => supervisor,
    modules => [hera_sup2]},

  SupervisorCalcMeas = #{id => hera_sup_calc_meas,
    start => {hera_sup2, start_link, [
      supervisor_calc_meas,
      one_for_one, [
        SupervisorMeasurements, SupervisorCalculation
      ]
    ]},
    restart => permanent,
    shutdown => 2000,
    type => supervisor,
    modules => [hera_sup2]},

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
    type => supervisor,
    modules => [hera_sup2]},

  SupervisorDispatch = #{id => hera_sup_dispatch,
    start => {hera_sup2, start_link, [
      supervisor_dispatch,
      simple_one_for_one, []
    ]},
    restart => permanent,
    shutdown => 2000,
    type => supervisor,
    modules => [hera_sup2]},

  Supervisor2 = #{id => hera_sup2,
    start => {hera_sup2, start_link, [
      supervisor_2,
      rest_for_one, [
        #{id => hera_global_sync,
          start => {hera_global_sync, start_link, []}},
        SupervisorDispatch
      ]
    ]},
    restart => permanent,
    shutdown => 2000,
    type => supervisor,
    modules => [hera_sup2]},

  {ok, {SupFlags, [SensorsData, Supervisor1, Supervisor2]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
