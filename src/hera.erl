% @doc hera public API.
% @end
-module(hera).
-author("Julien Bastin <julien.bastin@student.uclouvain.be>, Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>").

-behavior(application).

-include("hera.hrl").

%% API
-export([declare/4]).
-export([filter/1]).

% Callbacks
-export([start/0]).
-export([stop/0]).

%--- Callbacks -----------------------------------------------------------------

start() ->
  %{ok, _} = application:ensure_all_started(hera),
  hera_sup:start_link().
stop() -> ok.

%% ===================================================================
%% API
%% ===================================================================

%% -------------------------------------------------------------------
%% @doc
%%
%% @end
%% -------------------------------------------------------------------
-spec declare(
    Data:: term(),
    Filter_Fun:: fun((term()) -> boolean()),
    Category :: atom(),
    Frequency :: number()) -> hera:filter_task().
declare(Data, Filter_Fun, Category, Frequency) ->
    #{category=>Category,
      data=>Data,
      filter_fun=>Filter_Fun,
      frequency=>Frequency}.

%% -------------------------------------------------------------------
%% @doc
%%
%% @end
%% -------------------------------------------------------------------
-spec filter(hera:filter_task()) -> ok.
filter(Filter) ->
  hera_filter:filter(Filter).
