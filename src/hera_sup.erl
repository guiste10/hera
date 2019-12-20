% @doc hera top level supervisor.
% @end
-module(hera_sup).

-behavior(supervisor).

% API
-export([start_link/0]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

start_link() -> 
    io:format("hello world ~n", []),
    io:format("hello world ~n", []),
    io:format("hello world ~n", []),
    io:format("hello world ~n", []),
    io:format("hello world ~n", []),
    io:format("hello world ~n", []),
    io:format("hello world ~n", []),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%--- Callbacks -----------------------------------------------------------------

init([]) -> {ok, { {one_for_all, 0, 1}, []} }.
