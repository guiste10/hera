% @doc hera top level supervisor.
% @end
-module(hera_sup).
-author("Julien Bastin <julien.bastin@student.uclouvain.be>, Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>").

-behavior(supervisor).

-include("hera.hrl").

% API
-export([start_link/3]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

% {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
%% @doc Start a pool supervisor.
-spec start_link(Name :: atom(), Limit :: integer(), MFA :: tuple()) ->  % args will be passed to init callback
    {ok, pid()}
    | ignore
    | {error, {already_started, pid()}
    | {shutdown, term()}
    | term()}.
start_link(Name, Limit, MFA) -> 
    io:format("hera_sup started!"),
    supervisor:start_link(?MODULE, {Name, Limit, MFA}).

%--- Callbacks -----------------------------------------------------------------
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
%% @private
-spec init({Name :: atom(), Limit :: integer(), MFA :: tuple()}) ->
    {ok , {supervisor:sup_flags() , [supervisor:child_spec()]}}.
init({Name, Limit, MFA}) ->
    MaxRestart = 1,
    MaxTime = 3600,
    {ok, {{one_for_all, MaxRestart, MaxTime}, 
        [{serv,
            {hera_serv, start_link, [Name, Limit, self(), MFA]}, % !! supervisor pid passed to the server
            permanent,
            5000, % Shutdown time
            worker, % type (= worker because it's not a supervisor)
            [hera_serv]}]}}.
