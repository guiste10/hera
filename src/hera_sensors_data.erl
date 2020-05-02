%%%-------------------------------------------------------------------
%%% @author Julien Bastin <julien.bastin@student.uclouvain.be>
%%% @author Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% Module that stores the last measurement of the sensors in order to allow to perform calculation on it.
%%% This module also handles logging of measurements and calculations.
%%% @end
%%% Created : 09. Feb 2020 5:04 PM
%%%-------------------------------------------------------------------
-module(hera_sensors_data).
-author("Julien Bastin <julien.bastin@student.uclouvain.be>, Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>").

-behaviour(gen_server).

-include("hera.hrl").

%% API
-export([start_link/0]).
-export([store_data/4]).
-export([get_data/1]).
-export([log_measure/4]).
-export([log_calculation/4]).

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
  % dictionary contraining the last data of all nodes
  % with the form [{node_name, {seqnum, data}}]
  data :: #{atom() => dict:dict(atom(), {integer(), integer() | float()})}, %% TODO: modify
  measures_logger_configs :: ets:tid(),
  calculations_logger_configs :: ets:tid()
}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @private
%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @private
stop(Pid) ->
  gen_server:call(Pid, stop).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve the data of the sensors of all nodes
%%
%% @spec get_data() -> dict:dict(string(), {integer(), integer() | float()})
%% @end
%%--------------------------------------------------------------------
-spec get_data(Name :: atom()) -> dict:dict(string(), {integer(), integer() | float()}).
get_data(Name) ->
  gen_server:call(?MODULE, {get_data, Name}).

%% -------------------------------------------------------------------
%% @doc
%% Add a new data for the specified node
%%
%% @param Name The name/type of the data (e.g. temperature, sonar, humidity, ...)
%% @param Node The node who perform the measurement of the data
%% @param Seqnum The sequence number of the measured data
%% @param Data The data measured by the sensor
%%
%% @spec store_data(Name :: atom(), Node :: atom(), Seqnum :: integer(), Data :: integer() | float()) -> ok
%% @end
%% -------------------------------------------------------------------
-spec store_data(Name :: atom(), Node :: atom(), Seqnum :: integer(), Data :: integer() | float()) -> ok.
store_data(Name, Node, Seqnum, Data) ->
  gen_server:cast(?SERVER, {store_data, {Name, {Node, Seqnum, Data}}}).

%%--------------------------------------------------------------------
%% @doc
%% Log the given measure into a file with the same name as the node name
%%
%% @param Name The name/type of the measure (e.g. temperature, sonar, humidity, ...)
%% @param Node The node who perform the measurement of the data
%% @param Seqnum The sequence number of the measured data
%% @param Data The data measured by the sensor
%%
%% @spec log_measure(Name :: atom(), Node :: atom(), Seqnum :: integer(), Data :: integer() | float()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec log_measure(Name :: atom(), Node :: atom(), Seqnum :: integer(), Data :: integer() | float()) -> ok.
log_measure(Name, Node, Seqnum, Data) ->
  gen_server:cast(?SERVER, {log_measure, {Name, {Node, Seqnum, Data}}}).

%%--------------------------------------------------------------------
%% @doc
%% Log the given measure into a file with the same name as the node name
%%
%% @param Name The name of the calculation (e.g. position_calculation, temperature_median, ...)
%% @param Node The node who perform the calculation
%% @param Seqnum The sequence number of the calculation result
%% @param Result The result of the calculation
%%
%% @spec log_calculation(Name :: atom(), Node :: atom(), Seqnum :: integer(), Data :: integer() | float()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec log_calculation(Name :: atom(), Node :: atom(), Seqnum :: integer(), Result :: integer() | float()) -> ok.
log_calculation(Name, Node, Seqnum, Result) ->
  gen_server:cast(?SERVER, {log_calculation, {Name, {Node, Seqnum, Result}}}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{data = maps:new(), measures_logger_configs = ets:new(measures_loggers, [ordered_set, named_table]), calculations_logger_configs = ets:new(calculations_loggers, [ordered_set, named_table])}}.

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
handle_call({get_data, Name}, _From, State = #state{data = Data}) ->
  Rep = case maps:is_key(Name) of
          true -> {ok, maps:get(Name, Data)};
          false -> {error, "Not yet values for this name"}
  end,
  {reply, Rep, State};
handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({store_data, {Name, {Node, Seqnum, Measure}}}, State = #state{data = Data}) ->
  New_data = case maps:is_key(Name, Data) of
               false ->
                 Data#{Name => dict:new()};
               true ->
                 Data
  end,
  %% TODO: modify to take the name into account
  Dict = maps:get(Name, New_data),
  Dict2 = case dict:find(Node, Dict) of
            {ok, {S, _Data}} ->
              if
                S < Seqnum orelse Seqnum == 0 ->
                  dict:store(Node, {Seqnum, Measure}, Dict);
                true ->
                  Dict
              end;
            error ->
              dict:store(Node, {Seqnum, Measure}, Dict)
          end,
  {noreply, State#state{data = New_data#{Name => Dict2}}};
handle_cast({log_measure, {Name, {Node, Seqnum, Data}}}, State = #state{measures_logger_configs = Log_Conf}) ->
  check_handlers(Name, Node, Log_Conf, measures),
  logger:debug("~p ~p", [Seqnum, Data], #{domain => [measures, Node, Name]}), %% Log data to file
  {noreply, State};
handle_cast({log_calculation, {Name, {Node, Seqnum, Data}}}, State = #state{calculations_logger_configs = Log_Conf}) ->
  check_handlers(Name, Node, Log_Conf, calculations),
  logger:debug("~p ~p", [Seqnum, Data], #{domain => [calculations, Node, Name]}), %% Log data to file
  {noreply, State};
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
%% @doc check if a handler exists for the given Name, Node and Log_Type
check_handlers(Name, Node, Log_Conf, Log_Type) ->
  case ets:member(Log_Conf, Node) of
    % If a handler is not yet added for the given Node, Name and Log_Type, add a new handler with the given name
    false ->
      Node_conf = ets:new(Name, [ordered_set]),
      ets:insert(Log_Conf, {Node, Node_conf}),
      Config = configure_handler(Name, Node, Log_Type),
      ets:insert(Node_conf, {Name, Config});
    true ->
      %% if a handler exists for the given node, lookup through data names
      [{_Node, Conf}] = ets:lookup(Log_Conf, Node),
      case ets:member(Conf, Name) of
        %% if no handler exists for the given Name, create ones
        false ->
          Config = configure_handler(Name, Node, Log_Type),
          ets:insert(Conf, {Name, Config});
        true -> ok
      end
  end.

%% @private
%% @doc Configure the handle to log each data to one file named {Name}_{Node} in the directory {Log_Type}
configure_handler(Name, Node, Log_Type) ->
  File_name = atom_to_list(Name) ++ "_" ++ atom_to_list(Node),
  File_path = filename:join(atom_to_list(Log_Type), File_name),
  Config = #{
    filters =>
    [
      {debug, {fun logger_filters:level/2, {stop, neq, debug}}}, %% Only allow debug logs
      {Node, {fun logger_filters:domain/2, {stop, not_equal, [Log_Type, Node, Name]}}} %% Only allow debug logs for the domain [Log_Type, Node, Name]
    ],
    config => #{  file => File_path}, %% Measures will be logged to file {Log_Type}/{Name}_{Node}
    formatter => {logger_formatter  , #{single_line => true, max_size => 30, template => [msg, "\n"]}} %% Only log on one line the message
  },
  logger:add_handler(list_to_atom(File_name), logger_disk_log_h, Config), %% add the handler with the provided config
  Config.