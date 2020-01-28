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
%-export([start/2]).
%-export([stop/1]).
-compile({nowarn_export_all}).
-compile(export_all).

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
%%
%% @end
%% -------------------------------------------------------------------
launch_app() ->
  hera_pool:start_pool(pool1, 1, {hera_measure, start_link, []}),
  hera_pool:run(pool1, [1000]),
  hera_pool:start_pool(pool2, 1, {hera_position, start_link, []}),
  hera_pool:run(pool2, [1000]).



%% -------------------------------------------------------------------
%% @doc
%%
%% @end
%% -------------------------------------------------------------------

fake_sonar_get() ->
  rand:uniform(10).