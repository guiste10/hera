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
-export([start_link/4, stop/1, restart_calculation/4, restart_calculation/1, restart_calculation/3, pause_calculation/1]).

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
-spec(start_link(Name :: atom(), CalcFunction :: function(), Delay :: integer(), MaxIterations :: integer() | infinity) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name, CalcFunction, Delay, MaxIterations) ->
  gen_server:start_link(?MODULE, {Name, CalcFunction, Delay, MaxIterations}, []).

%% @private
stop(Pid) ->
  gen_server:call(Pid, stop).

%%--------------------------------------------------------------------
%% @doc
%% Restart workers that performs the calculations
%%
%% @param Name The name of the measurement
%% @param Func The calculation function to be executed
%% @param Args The arguments of the function
%% @param Frequency The frequency of the calculation
%% @param MaxIterations The number of iterations to be done
%%
%% @spec restart_calculation(Name :: atom(), Func :: fun((...) -> {ok, term()} | {error, term()}), Args :: list(any()), Frequency :: integer(), MaxIterations :: integer()) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec restart_calculation(Name :: atom(), Func :: fun((...) -> {ok, term()} | {error, term()}), Frequency :: integer(), MaxIterations :: integer()) -> ok.
restart_calculation(Name, Func, Frequency, MaxIterations) ->
  gen_server:cast(Name, {restart, {Func, Frequency, MaxIterations}}).

%%--------------------------------------------------------------------
%% @doc
%% Restart worker that performs the calculation <Name>
%%
%% @param Name The name of the calculation
%%
%% @spec restart_calculation(Name :: atom()) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec restart_calculation(Name :: atom()) -> ok.
restart_calculation(Name) ->
  gen_server:cast(Name, restart).

%%--------------------------------------------------------------------
%% @doc
%% Restart worker that performs the calculation <Name>
%%
%% @param Name The name of the calculation
%% @param Frequency The frequency of the calculation
%% @param MaxIterations The number of iterations to be done
%%
%% @spec restart_calculation(Name :: atom(), Frequency :: integer(), MaxIterations :: integer() | infinity) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec restart_calculation(Name :: atom(), Frequency :: integer(), MaxIterations :: integer() | infinity) -> ok.
restart_calculation(Name, Frequency, MaxIterations) ->
  gen_server:cast(Name, {restart, {Frequency, MaxIterations}}).

%%--------------------------------------------------------------------
%% @doc
%% Pause the worker that performs the calculation <Name>
%%
%% @param Name The name of the calculation <name>
%%
%% @spec pause_calculation(Name :: atom()) -> ok.
%% @end
%%--------------------------------------------------------------------
pause_calculation(Name) ->
  gen_server:cast(Name, pause).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init({Name :: atom(), Calc_function :: function(), Args :: list(any()), Delay :: integer(), MaxIterations :: integer() | infinity}) ->
  {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init({Name, CalcFunction, Args, Delay, MaxIterations}) ->
  {ok, #state{name = Name, calc_function = CalcFunction, delay = Delay, iter = 0, max_iterations = MaxIterations}, Delay}.

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
handle_cast(restart, State = #state{delay = Delay}) ->
  {noreply, State, Delay};
handle_cast({restart, {Frequency, MaxIterations}}, State) ->
  {noreply, State#state{iter = 0, max_iterations = MaxIterations, delay = Frequency}, Frequency};
handle_cast({restart, {Func, Args, Delay, MaxIter}}, State) ->
  {noreply, State#state{iter = 0, calc_function = Func, max_iterations = MaxIter, delay = Delay}, Delay};
handle_cast(pause, State) ->
  {noreply, State, hibernate};
handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: state()) ->
  {noreply, NewState :: state()} |
  {noreply, NewState :: state(), timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: state()}).
handle_info(timeout, State = #state{name = Name, calc_function = Func, iter = Iter, delay = Delay, max_iterations = MaxIterations}) ->
  case Func() of
    {error, Reason} -> logger:error(Reason);
    {ok, Res} -> hera:send(calc, Name, node(), Iter, Res);
    Other -> io:format("result : ~p", [Other])
  end,
  case MaxIterations of
    Iter -> {noreply, State#state{iter = 0}, hibernate};
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
