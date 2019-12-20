% @doc hera public API.
% @end
-module(hera).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) -> hera_sup:start_link().

stop(_State) -> ok.
