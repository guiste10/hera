%%====================================================================
%% @doc hera constants definitions
%% @end
%%====================================================================
-export_type([calculation/0, sync_measurement/0, unsync_measurement/0]).
%%====================================================================
%% Common Macros
%%====================================================================

-define(MAX_SEQNUM, 1023).
-define(SYNC_PROC, hera_global_sync).

%%====================================================================
%% Types
%%====================================================================

%% @doc
%% Type of a calculation to be perform by the application
%% @type calculation() :: {atom(), #{func => function(), args => list(any()), frequency => integer()}}
-type calculation() ::
  {CalculationName :: atom(),
    #{func => CalculationFunction :: fun(() -> {ok, term()} | {error, term()}),
      frequency => CalculationFrequency :: integer(),
      max_iterations => MaxIterations :: integer() | infinity,
      upper_bound => float(),
      filter => FilteringFunction :: fun((any(), any(), integer(), list(any())) -> boolean()) | undefine
    }
  }.

%% @doc
%% Type of a measurement to be perform by the application
%% @type sync_measurement() :: {atom(), #{func => function(), filtering => boolean(), max_iterations => integer() | infinity, synchronization => true}}
-type sync_measurement() ::
  {MeasurementName :: atom(),
    #{func => MeasurementFunction :: fun(() -> {ok, term()} | {error, term()}),
      filter => FilteringFunction :: fun((any(), any(), integer(), list(any())) -> boolean()) | undefined,
      upper_bound => float(),
      max_iterations => MaxIterations :: integer() | infinity,
      synchronization => MakeSynchronisationOfMeasurements :: true
    }
  }.

%% @doc
%% Type of a measurement to be perform by the application
%% @type measurement() :: {atom(), #{func => function(), args => list(any()), frequency => integer(), filtering => boolean()}}
-type unsync_measurement() ::
{MeasurementName :: atom(),
  #{func => MeasurementFunction :: fun(() -> {ok, term()} | {error, term()}),
    frequency => CalculationFrequency :: integer(),
    filter => FilteringFunction :: fun((any(), any(), integer(), list(any())) -> boolean()) | undefined,
    upper_bound => float(),
    max_iterations => MaxIterations :: integer() | infinity,
    synchronization => MakeSynchronisationOfMeasurements :: false
  }
}.