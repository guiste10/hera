% @doc hera top level supervisor.
% @end
-module(hera_supersup).
-author("Julien Bastin <julien.bastin@student.uclouvain.be>, Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>").

-behavior(supervisor).

-include("hera.hrl").

% API
-export([start_link/0, stop/0, start_pool/3, stop_pool/1]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

% {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
%% @doc Start hera top level supervisor using the current supervisor (= shell)
-spec start_link() ->
    {ok, pid()}
    | ignore
    | {error, {already_started, pid()}
    | {shutdown, term()}
    | term()}.
start_link() -> 
    io:format("supersup process is being started ~n"),
    supervisor:start_link({local, supersup}, ?MODULE, []).

%% @doc  kill supervisor brutally
-spec stop() ->
    true
    | ok.
stop() ->
    case whereis(supersup) of
    P when is_pid(P) ->
    exit(P, kill);
    _ -> ok
    end.

% {ok, Child :: child()} |
% {ok, Child :: child(), Info :: term()} |
% {error, startchild_err()}.

%% @doc starts a process pool named "Name", with num of workers limit "limit", using worker specified in MFA
% worker will be started by worker_sup = its supervisor
-spec start_pool(Name :: atom(), Limit :: integer(), MFA :: tuple()) ->
    supervisor:startchild_ret().
start_pool(Name, Limit, MFA) ->
    ChildSpec = {Name, % id
    {hera_sup, start_link, [Name, Limit, MFA]}, % start
    permanent, 10500, supervisor, [hera_sup]}, % restart,shutdown,type,module
    supervisor:start_child(supersup, ChildSpec). % supersup will be the supervisor, hera_sup:startlink will be called by supersup, supersup will be considered as the supervisor in hera_sup:startlink

%% @doc stops a process pool
-spec stop_pool(PoolName :: atom()) ->
    ok | 
    {error, running | restarting | not_found | simple_one_for_one}.
stop_pool(Name) ->
    supervisor:terminate_child(supersup, Name),
    supervisor:delete_child(supersup, Name).

%--- Callbacks -----------------------------------------------------------------
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
%% @private
-spec init([]) ->
    {ok , {supervisor:sup_flags() , []}}.
init([]) ->
    MaxRestart = 6,
    MaxTime = 3600,
    {ok, {{one_for_one, MaxRestart, MaxTime}, []}}. % childless