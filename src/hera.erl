% @doc hera public API.
% @end
-module(hera).
-author("Julien Bastin <julien.bastin@student.uclouvain.be>, Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>").

-behavior(application).

-include("hera.hrl").

%% API
%-export([declare/4]).
%-export([filter/1]).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) ->
  %{ok, _} = application:ensure_all_started(hera),
  %application:start(kernel),
  %application:start(stdlib),
  hera_supersup:start_link(). % verif bon appel?

stop(_State) -> ok.

%% ===================================================================
%% API
%% ===================================================================

%% -------------------------------------------------------------------
%% @doc
%%
%% @end
%% -------------------------------------------------------------------


%% -------------------------------------------------------------------
%% @doc
%%
%% @end
%% -------------------------------------------------------------------

