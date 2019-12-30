% @doc hera top level supervisor.
% @end
-module(hera_sup).
-author("Julien Bastin <julien.bastin@student.uclouvain.be>, Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>").

-behavior(supervisor).

-include("hera.hrl").

% API
-export([start_link/0]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

% {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
%% @doc Start hera top level supervisor.
-spec start_link() ->
    {ok, pid()}
    | ignore
    | {error, {already_started, pid()}
    | {shutdown, term()}
    | term()}.
start_link() -> 
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%--- Callbacks -----------------------------------------------------------------
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
%% @private
-spec init(term()) ->
    {ok , {supervisor:sup_flags() , [supervisor:child_spec()]}}.
init([]) -> 
    {ok, { 
        ?SUPFLAGS(5, 25), [
            
        ]} 
    }.
