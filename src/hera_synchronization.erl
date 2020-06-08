%%%-------------------------------------------------------------------
%%% @author julien
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jun 2020 5:27 PM
%%%-------------------------------------------------------------------
-module(hera_synchronization).
-author("julien").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-export([send_measurement_phase_information/2, send_measurement_phase/1, is_in_measurement_phase/1, update_measurement_phase/2]).
-export([update_order/2, get_order/1]).

-define(SERVER, ?MODULE).

-record(state, {
  name :: atom(),
  node_order :: queue:queue(),
  measurement_phase :: boolean()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Name :: atom()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name) ->
  gen_server:start_link(?MODULE, Name, []).

send_measurement_phase_information(Name, Phase) ->
  gen_server:call(hera:get_registered_name(Name, "syn"), {send_meas_phase, Phase}).

get_order(Name) ->
  gen_server:call(hera:get_registered_name(Name, "syn"), get_order).

update_order(Name, Order) ->
  gen_server:call(hera:get_registered_name(Name, "syn"), {update_order, Order}).

%%--------------------------------------------------------------------
%% @doc
%% Return true if the node is in measurement phase for measurement <Name>
%%
%% @param Name The name of the measurement
%%
%% @spec is_in_measurement_phase(Name :: atom()) -> boolean().
%% @end
%%--------------------------------------------------------------------
-spec is_in_measurement_phase(Name :: atom()) -> boolean().
is_in_measurement_phase(Name) ->
  gen_server:call(hera:get_registered_name(Name, "syn"), is_in_measurement).

-spec update_measurement_phase(Name :: atom(), Phase :: boolean()) -> any().
update_measurement_phase(Name, Phase) ->
  gen_server:call(hera:get_registered_name(Name, "syn"), {update_meas_phase, Phase}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(Name) ->
  {Pid, _Ref} = spawn_opt(?SERVER, send_measurement_phase, [Name], [monitor]),
  register(hera:get_registered_name(Name, "mp"), Pid),
  {ok, #state{name = Name, node_order = queue:new(), measurement_phase = false}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({send_meas_phase, Phase}, _From, State = #state{name = Name, node_order = Order}) ->
  hera_multicast:send({measurement_phase, Name, Phase, node(), Order}),
  {reply, ok, State};
handle_call(get_order, _From, State = #state{node_order = Order}) ->
  {reply, Order, State};
handle_call({update_order, Order}, _From, State) ->
  {reply, ok, State#state{node_order = Order}};
handle_call(is_in_measurement, _From, State = #state{measurement_phase = MP}) ->
  {reply, MP, State};
handle_call({update_meas_phase, Phase}, _From, State) ->
  {reply, ok, State#state{measurement_phase = Phase}};
handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State = #state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State = #state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc send every 40ms if the node is in measurement phase or not
send_measurement_phase(Name) ->
%%  hera_synchronization:send_measurement_phase_information(Name, hera_synchronization:is_in_measurement_phase(Name)),
  hera_multicast:send({measurement_phase, Name, hera_synchronization:is_in_measurement_phase(Name), node(), hera_synchronization:get_order(Name)}),
  timer:sleep(40),
  send_measurement_phase(Name).