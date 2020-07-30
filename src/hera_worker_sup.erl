-module(hera_worker_sup).
-export([start_link/1, start_link/2, init/1]).
-behaviour(supervisor).
 
-spec start_link({Module :: atom(), Function :: atom(), Arguments :: list()}) ->
    {ok, pid()}
    | ignore
    | {error, {already_started, pid()}
    | {shutdown, term()}
    | term()}.
start_link(MFA = {M,_,_}) ->
    supervisor:start_link({local, hera_utils:concat_atoms(worker_sup_, M)}, ?MODULE, MFA).

start_link(MFA = {M,_,_}, Suffix) ->
    supervisor:start_link({local, hera_utils:concat_atoms(hera_utils:concat_atoms(worker_sup_, M), Suffix)}, ?MODULE, MFA).


-spec init({Module :: atom(), Function :: atom(), Arguments :: list()}) ->
    {ok , {supervisor:sup_flags() , [supervisor:child_spec()]}}.
init({M,F,A}) ->
    MaxRestart = 5,
    MaxTime = 3600,
    {ok, {{simple_one_for_one, MaxRestart, MaxTime},
        [{ppool_worker, % child id, used to identify the child specification internally by the supervisor heraworkersup
        {M,F,A},
        permanent, 5000, worker, [M]}]}}.