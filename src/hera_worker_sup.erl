-module(hera_worker_sup).
-export([start_link/1, init/1]).
-behaviour(supervisor).
 
-spec start_link({Module :: atom(), Function :: atom(), Arguments :: list()}) ->
    {ok, pid()}
    | ignore
    | {error, {already_started, pid()}
    | {shutdown, term()}
    | term()}.
start_link(MFA = {_,_,_}) ->
    supervisor:start_link(?MODULE, MFA).
 
-spec init({Module :: atom(), Function :: atom(), Arguments :: list()}) ->
    {ok , {supervisor:sup_flags() , [supervisor:child_spec()]}}.
init({M,F,A}) ->
    MaxRestart = 5,
    MaxTime = 3600,
    Restart = case M of
                  hera_calculation -> temporary;
                  hera_measure -> temporary;
                  _ -> permanent
              end,
    {ok, {{simple_one_for_one, MaxRestart, MaxTime},
        [{ppool_worker, % child id, used to identify the child specification internally by the supervisor heraworkersup
        {M,F,A},
        Restart, 5000, worker, [M]}]}}.