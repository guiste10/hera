%%====================================================================
%% @doc hera constants definitions
%% @end
%%====================================================================
-export_type([calculation/0, measurement/0]).
%%====================================================================
%% Common Macros
%%====================================================================

-define(MAX_SEQNUM, 1023).

%%====================================================================
%% Types
%%====================================================================

%% @doc
%% Type of a calculation to be perform by the application
%% @type calculation() :: {atom(), #{func => function(), args => list(any()), frequency => integer()}}
-type calculation() :: {atom(), #{func => fun((...) -> {ok, term()} | {error, term()}), args => list(any()), frequency => integer()}}.

%% @doc
%% Type of a measurement to be perform by the application
%% @type measurement() :: {atom(), #{func => function(), args => list(any()), frequency => integer()}}
-type measurement() :: {atom(), #{func => fun((...) -> {ok, term()} | {error, term()}), args => list(any()), frequency => integer()}}.