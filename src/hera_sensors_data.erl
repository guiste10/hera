%%%-------------------------------------------------------------------
%%% @author julien
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Feb 2020 5:04 PM
%%%-------------------------------------------------------------------
-module(hera_sensors_data).
-author("julien").

-behaviour(gen_server).

-include("hera.hrl").

%% API
-export([start_link/0]).
-export([store_data/3]).
-export([get_data/0]).
-export([log_measure/3]).

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
  data :: dict:dict(string(), {integer(), integer() | float()}),
  logger_configs :: ets:tid()
}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve the data of the sensors of all nodes
%%
%% @spec get_data() -> dict:dict(string(), {integer(), integer() | float()})
%% @end
%%--------------------------------------------------------------------
-spec get_data() -> dict:dict(string(), {integer(), integer() | float()}).
get_data() ->
  gen_server:call(?MODULE, get_data).

%%--------------------------------------------------------------------
%% @doc
%% Update the state with a new value of the data
%%
%% @spec store_data(Node :: string(), Seqnum :: integer(), Data :: integer() | float()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec store_data(Node :: string(), Seqnum :: integer(), Data :: integer() | float()) -> ok.
store_data(Node, Seqnum, Data) ->
  gen_server:cast(?SERVER, {store_data, {Node, Seqnum, Data}}).

%%--------------------------------------------------------------------
%% @doc
%% Log the given measure into a file with the same name as the node name
%%
%% @spec store_data(Node :: string(), Seqnum :: integer(), Data :: integer() | float()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec log_measure(Node :: string(), Seqnum :: integer(), Data :: integer() | float()) -> ok.
log_measure(Node, Seqnum, Data) ->
  gen_server:cast(?SERVER, {log_measure, {Node, Seqnum, Data}}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{data = dict:new(), logger_configs = ets:new(nodes_logger, [ordered_set, named_table])}}.

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
handle_call(get_data, _From, State) ->
  {reply, State#state.data, State};
handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({store_data, {Node, Seqnum, Data}}, State) ->
  Dict2 = case dict:find(Node, State#state.data) of
            {ok, {S, _Data}} ->
              if
                S < Seqnum orelse Seqnum == 0 ->
                  dict:store(Node, {Seqnum, Data}, State#state.data);
                true ->
                  State#state.data
              end;
            error ->
              dict:store(Node, {Seqnum, Data}, State#state.data)
          end,
  {noreply, State#state{data = Dict2}};
handle_cast({log_measure, {Node, Seqnum, Data}}, State) ->
  case ets:member(State#state.logger_configs, Node) of
    % If a handler is not yet added for the given Node, add a new handler
    false ->
      File_Name = "measures/" ++ atom_to_list(Node),
      Config = #{
        filters =>
        [
          {debug, {fun logger_filters:level/2, {stop, neq, debug}}}, %% Only allow debug logs
          {Node, {fun logger_filters:domain/2, {stop, not_equal, [Node]}}} %% Only allow debug logs for the domain {Node}
        ],
        config => #{  file => File_Name}, %% Measures will be logged to file logs/{Node}
        formatter => {logger_formatter  , #{single_line => true, max_size => 30, template => [msg, "\n"]}} %% Only log on one line the message
      },
      logger:add_handler(Node, logger_disk_log_h, Config), %% add the handler with the provided config
      ets:insert(State#state.logger_configs, {Node, Config});
    true -> ok
  end,
  logger:debug("~p ~p", [Seqnum, Data], #{domain => [Node]}), %% Log data to file
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
