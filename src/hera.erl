% @doc hera public API.
% @end
-module(hera).

-behavior(application).

% Application callbacks
-export([make_some_action/1]).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) -> hera_sup:start_link().

stop(_State) -> ok.

-spec make_some_action(Func::function()) -> erlang:function().
make_some_action(Func) ->
    io:format('start to make some hera action'),
    Func.