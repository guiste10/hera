%%%-------------------------------------------------------------------
%%% @author julien
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Dec 2019 11:18 AM
%%%-------------------------------------------------------------------
-module(hera_filter).
-author("Julien Bastin <julien.bastin@student.uclouvain.be>, Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>").

-behaviour(gen_server).

-include("hera.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(hera_filter_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #hera_filter_state{}} | {ok, State :: #hera_filter_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #hera_filter_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #hera_filter_state{}) ->
  {reply, Reply :: term(), NewState :: #hera_filter_state{}} |
  {reply, Reply :: term(), NewState :: #hera_filter_state{}, timeout() | hibernate} |
  {noreply, NewState :: #hera_filter_state{}} |
  {noreply, NewState :: #hera_filter_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #hera_filter_state{}} |
  {stop, Reason :: term(), NewState :: #hera_filter_state{}}).
handle_call(_Request, _From, State = #hera_filter_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #hera_filter_state{}) ->
  {noreply, NewState :: #hera_filter_state{}} |
  {noreply, NewState :: #hera_filter_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #hera_filter_state{}}).
handle_cast(_Request, State = #hera_filter_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #hera_filter_state{}) ->
  {noreply, NewState :: #hera_filter_state{}} |
  {noreply, NewState :: #hera_filter_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #hera_filter_state{}}).
handle_info(_Info, State = #hera_filter_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #hera_filter_state{}) -> term()).
terminate(_Reason, _State = #hera_filter_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #hera_filter_state{},
    Extra :: term()) ->
  {ok, NewState :: #hera_filter_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #hera_filter_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
