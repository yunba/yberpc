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

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

-ifdef(NODEBUG).
-define(LOG_DBG(Fmt, Args), lager:debug(Fmt, Args)).
-define(LOG_ERR(Fmt, Args), lager:error(Fmt, Args)).
-else.
-define(LOG_DBG(Fmt, Args), ?debugFmt(Fmt, Args)).
-define(LOG_ERR(Fmt, Args), ?debugFmt(Fmt, Args)).
-endif.

%% API
-export([start_link/0,
  start_server/2,
  start_client/2,
  rpc/2,
  stop_server/1,
  stop_client/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-define(RPC_TIMEOUT, 500).

-record(state, {
  servers = [],
  clients = []
}).

%%%===================================================================
%%% API
%%%===================================================================

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

start_server(Url, Handler) ->
  gen_server:call(?MODULE, {start_server, Url, Handler}).

start_client(Url, Handler) ->
  gen_server:call(?MODULE, {start_client, Url, Handler}).

rpc(Sock, ReqData) ->
  gen_server:call(?MODULE, {rpc, Sock, ReqData}).

stop_server(Sock) ->
  gen_server:call(?MODULE, {stop_server, Sock}).

stop_client(Sock) ->
  gen_server:call(?MODULE, {stop_client, Sock}).

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

handle_call({start_server, Url, Handler}, _From, #state{servers = Servers} = State) ->
  ?LOG_DBG("start_server: ~p", [Url]),
  case enm:rep([{bind, Url}, {nodelay, true}]) of
    {ok, Sock} ->
      ?LOG_DBG("server sock: ~p", [Sock]),
      Servers2 = lists:append(Servers, [{Sock, Handler}]),
      {reply, {ok, Sock}, State#state{servers = Servers2}};
    Else ->
      ?LOG_DBG("enm:rep error: ~p", [Else]),
      {reply, {error, Else}, State}
  end;

%%  https://github.com/basho/enm/issues/7
handle_call({start_client, Url, Handler}, _From, #state{clients = Clients} = State) ->
  ?LOG_DBG("start_client: ~p", [Url]),
  case enm:req([{connect, Url}, {nodelay, true}]) of
    {ok, Sock} ->
      ?LOG_DBG("client sock: ~p", [Sock]),
      Clients2 = lists:append(Clients, [{Sock, Handler}]),
      {reply, {ok, Sock}, State#state{clients = Clients2}};
    Else ->
      ?LOG_DBG("enm:req: ~p", [Else]),
      {reply, {error, Else}, State}
  end;

handle_call({rpc, Sock, ReqData}, _From, State) ->
  ?LOG_DBG("rpc sock: ~p", [Sock]),
  case send_data(Sock, ReqData) of
    ok ->
      case recv_data(Sock, <<0>>) of
        ok ->
          {reply, ok, State};
        Else ->
          {reply, Else, State}
      end;
    Else ->
      {reply, Else, State}
  end;

handle_call({stop_client, Sock}, _From, #state{clients = Clients} = State) ->
  ?LOG_DBG("stop_client: ~p", [Sock]),
  case lists:keyfind(Sock, 1, Clients) of
    false ->
      {reply, ok, State};
    Client ->
      ?LOG_DBG("close: ~p", [Sock]),
      enm:close(Sock),
      Clients2 = lists:delete(Client, Clients),
      {reply, ok, State#state{clients = Clients2}}
  end;

handle_call({stop_server, Sock}, _From, #state{servers = Servers} = State) ->
  ?LOG_DBG("stop_server: ~p", [Sock]),
  case lists:keyfind(Sock, 1, Servers) of
    false ->
      {reply, ok, State};
    Server ->
      ?LOG_DBG("close: ~p", [Sock]),
      enm:close(Sock),
      Servers2 = lists:delete(Server, Servers),
      {reply, ok, State#state{clients = Servers2}}
  end;

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

handle_info({nnrep, Sock, Data}, #state{servers = Servers} = State) ->
  ?LOG_DBG("receive a nnrep: ~p", [Sock]),
  case lists:keyfind(Sock, 1, Servers) of
    false ->
      {noreply, State};
    {_Sock, Handler} ->
      Handler ! {rpc_proxy_req, {Sock, Data}},
      send_data(Sock, <<0>>),
      {noreply, State}
  end;

handle_info({nnreq, Sock, Data}, #state{clients = Clients} = State) ->
  ?LOG_DBG("receive a nnreq: ~p", [Sock]),
  case lists:keyfind(Sock, 1, Clients) of
    false ->
      {noreply, State};
    {_Sock, Handler} ->
      Handler ! {rpc_proxy_rep, {Sock, Data}},
      {noreply, State}
  end;

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
send_data(Sock, ReqData) ->
  case enm:send(Sock, ReqData) of
    ok ->
      ok;
    Else ->
      ?LOG_ERR("enm:send error: sock: ~p, error: ~p", [Sock, Else]),
      Else
  end.

recv_data(Sock, RepData) ->
  receive
    {nnreq, Sock, RepData} ->
      ok
  after ?RPC_TIMEOUT ->
    ?LOG_ERR("recv_data recv ack timeout: ~p", [Sock]),
    timeout
  end.