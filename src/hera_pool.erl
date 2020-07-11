%%% API module for the pool
-module(hera_pool).
-export([start_link/0, stop/0, start_pool/3,
run/2, sync_queue/2, async_queue/2, stop_pool/1, set_limit/2]).

start_link() ->
    hera_supersup:start_link().

stop() ->
    hera_supersup:stop().

start_pool(Name, Limit, {M,F,A}) ->
    hera_supersup:start_pool(Name, Limit, {M,F,A}).

stop_pool(Name) ->
    hera_supersup:stop_pool(Name).

run(Name, Args) ->
    hera_serv:run(Name, Args).

set_limit(Name, Limit) ->
    hera_serv:set_limit(Name, Limit).
 
async_queue(Name, Args) ->
    hera_serv:async_queue(Name, Args).
 
sync_queue(Name, Args) ->
    hera_serv:sync_queue(Name, Args).