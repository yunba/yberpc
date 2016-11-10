%%%-------------------------------------------------------------------
%%% @author shdxiang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Nov 2016 5:00 PM
%%%-------------------------------------------------------------------
-module(msgbus_rpc_proxy).
-author("shdxiang").

-behaviour(gen_server).

-compile({parse_transform, lager_transform}).

%% API
-export([start_link/0,
  start_client/1,
  stop_client/1,
  start_server/1,
  stop_server/1,
  join_handler/1,
  leave_handler/1,
  sync_request/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_client(Url) ->
  gen_server:call(?SERVER, {start_client, Url}).

stop_client(Sock) ->
  gen_server:call(?SERVER, {stop_client, Sock}).

start_server(Url) ->
  gen_server:call(?SERVER, {start_server, Url}).

stop_server(Sock) ->
  gen_server:call(?SERVER, {stop_server, Sock}).

join_handler(Pid) ->
  gen_server:call(?SERVER, {join_handler, Pid}).

leave_handler(Pid) ->
  gen_server:call(?SERVER, {leave_handler, Pid}).

sync_request(Sock, ReqData) ->
  gen_server:call(?SERVER, {sync_request, Sock, ReqData}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  io:format("======init"),
  enm:start_link(),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call({start_client, Url}, _From, State) ->
  io:format("======start_client: ~p~n", [Url]),
  {ok, Sock} = enm:req([{connect, Url}]),
  {reply, {ok, Sock}, State};

handle_call({stop_client, Sock}, _From, State) ->
  io:format("======stop_client: ~p~n", [Sock]),
  {ok, Sock} = enm:close(Sock),
  {reply, ok, State};

handle_call({start_server, Url}, _From, State) ->
  io:format("======start_server: ~p~n", [Url]),
  {ok, Sock} = enm:rep([{bind, Url}]),
  pg2:create(msgbus_rpc_proxy_handler_pg2),
  {reply, {ok, Sock}, State};

handle_call({stop_server, Sock}, _From, State) ->
  io:format("======stop_server: ~p~n", [Sock]),
  {ok, Sock} = enm:close(Sock),
  pg2:delete(msgbus_rpc_proxy_handler_pg2),
  {reply, ok, State};

handle_call({join_handler, Pid}, _From, State) ->
  io:format("======join_handler: ~p~n", [Pid]),
  pg2:join(msgbus_rpc_proxy_handler_pg2, Pid),
  Pid2 = pg2:get_closest_pid(msgbus_rpc_proxy_handler_pg2),
  {reply, ok, State};

handle_call({leave_handler, Pid}, _From, State) ->
  io:format("======leave_handler: ~p~n", [Pid]),
  pg2:leave(msgbus_rpc_proxy_handler_pg2, Pid),
  {reply, ok, State};

handle_call({sync_request, Sock, ReqData}, _From, State) ->
  io:format("======sync_request: ~p~n", [Sock]),
  ok = enm:send(Sock, ReqData),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info({nnrep, _Rep, ReqData}, State) ->
  io:format("======receive a nnrep~n"),
  Pid = pg2:get_closest_pid(msgbus_rpc_proxy_handler_pg2),
  gen_server:cast(Pid, {rpc_req_data, ReqData}),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  lager:debug("terminate"),
%%  enm:stop(),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
