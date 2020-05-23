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
-export([start_link/5, stop/1, restart_calculation/5, restart_calculation/1, restart_calculation/3, pause_calculation/1]).

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

%%--------------------------------------------------------------------
%% @doc
%% Restart workers that performs the calculations
%%
%% @param Name The name of the measurement
%% @param Func The calculation function to be executed
%% @param Args The arguments of the function
%% @param Frequency The frequency of the calculation
%% @param Max_iterations The number of iterations to be done
%%
%% @spec restart_calculation(Name :: atom(), Func :: fun((...) -> {ok, term()} | {error, term()}), Args :: list(any()), Frequency :: integer(), Max_iterations :: integer()) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec restart_calculation(Name :: atom(), Func :: fun((...) -> {ok, term()} | {error, term()}), Args :: list(any()), Frequency :: integer(), Max_iterations :: integer()) -> ok.
restart_calculation(Name, Func, Args, Frequency, Max_iterations) ->
  gen_server:cast(Name, {restart, {Func, Args, Frequency, Max_iterations}}).

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
%% @param Max_iterations The number of iterations to be done
%%
%% @spec restart_calculation(Name :: atom(), Frequency :: integer(), Max_iterations :: integer() | infinity) -> ok.
%% @end
%%--------------------------------------------------------------------
-spec restart_calculation(Name :: atom(), Frequency :: integer(), Max_iterations :: integer() | infinity) -> ok.
restart_calculation(Name, Frequency, Max_iterations) ->
  gen_server:cast(Name, {restart, {Frequency, Max_iterations}}).

%%--------------------------------------------------------------------
%% @doc
%% Pause the worker that performs the calculation <Name>
%%
%% @param Name The name of the calculation
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
handle_cast(restart, State = #state{delay = Delay}) ->
  {noreply, State, Delay};
handle_cast({restart, {Frequency, Max_iterations}}, State) ->
  {noreply, State#state{iter = 0, max_iterations = Max_iterations, delay = Frequency}, Frequency};
handle_cast({restart, {Func, Args, Delay, Max_iter}}, State) ->
  {noreply, State#state{iter = 0, calc_function = Func, func_args = Args, max_iterations = Max_iter, delay = Delay}, Delay};
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
handle_info(timeout, State = #state{name = Name, calc_function = Func, func_args = Args, iter = Iter, delay = Delay, max_iterations = Max_iterations}) ->
  case erlang:apply(Func, Args) of
    {error, Reason} -> logger:error(Reason);
    {ok, Res} -> hera:send(calc, Name, node(), Iter, Res);
    Other -> io:format("result : ~p", [Other])
  end,
  case Max_iterations of
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
