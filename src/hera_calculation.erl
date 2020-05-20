%%%-------------------------------------------------------------------
%%% @author Julien Bastin <julien.bastin@student.uclouvain.be>
%%% @author Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% Module called to perform some calculations at a certain frequency
%%% @end
%%% Created : 01. May 2020 5:55 PM
%%%-------------------------------------------------------------------
-module(hera_calculation).
-author("Julien Bastin <julien.bastin@student.uclouvain.be>, Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>").

-behaviour(gen_server).

-include("hera.hrl").

%% API
-export([start_link/5, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%%====================================================================
%% Macros
%%====================================================================

-define(SERVER, ?MODULE).

%%====================================================================
%% Records
%%====================================================================


-record(state, {
  name :: atom(),
  calc_function :: function(),
  func_args :: list(any()),
  delay :: integer(),
  iter :: integer(),
  max_iterations :: integer() | infinity
}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @private
%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Name :: atom(), Calc_function :: function(), Args :: list(any()), Delay :: integer(), Max_iterations :: integer() | infinity) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name, Calc_function, Args, Delay, Max_iterations) ->
  gen_server:start_link(?MODULE, {Name, Calc_function, Args, Delay, Max_iterations}, []).

%% @private
stop(Pid) ->
  gen_server:call(Pid, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init({Name :: atom(), Calc_function :: function(), Args :: list(any()), Delay :: integer(), Max_iterations :: integer() | infinity}) ->
  {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init({Name, Calc_function, Args, Delay, Max_iterations}) ->
  {ok, #state{name = Name, calc_function = Calc_function, func_args = Args, delay = Delay, iter = 0, max_iterations = Max_iterations}, Delay}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: state()) ->
  {reply, Reply :: term(), NewState :: state()} |
  {reply, Reply :: term(), NewState :: state(), timeout() | hibernate} |
  {noreply, NewState :: state()} |
  {noreply, NewState :: state(), timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: state()} |
  {stop, Reason :: term(), NewState :: state()}).
handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: state()) ->
  {noreply, NewState :: state()} |
  {noreply, NewState :: state(), timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: state()}).
handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: state()) ->
  {noreply, NewState :: state()} |
  {noreply, NewState :: state(), timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: state()}).
handle_info(timeout, State = #state{name = Name, calc_function = Func, func_args = Args, iter = Iter, delay = Delay, max_iterations = Max_iterations}) ->
  case erlang:apply(Func, Args) of
    {error, Reason} -> logger:error(Reason);
    {ok, Res} -> hera:send(calc, Name, node(), Iter, Res);
    Other -> io:format("result : ~p", [Other])
  end,
  case Max_iterations of
    Iter -> {stop, normal, State};
    _ -> {noreply, State#state{iter = Iter+1 rem ?MAX_SEQNUM}, Delay}
  end;
handle_info(_Info, State = #state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: state()) -> term()).
terminate(_Reason, _State = #state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: state(),
    Extra :: term()) ->
  {ok, NewState :: state()} | {error, Reason :: term()}).
code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
