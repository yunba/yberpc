%%%-------------------------------------------------------------------
%%% @author shdxiang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Nov 2016 5:00 PM
%%%-------------------------------------------------------------------
-module(yberpc).
-author("shdxiang").

-behaviour(gen_server).

-compile({parse_transform, lager_transform}).

%% API
-export([start_link/1,
  start_server/2,
  start_client/1,
  request/2,
  reply/2,
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

-define(RPC_TIMEOUT, 1000).

-record(state, {
  sock,
  handler
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
-spec(start_link(Args :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Args) ->
  gen_server:start_link(?MODULE, [Args], []).

start_server(Url, Handler) ->
  supervisor:start_child(yberpc_sup, [{server, Url, Handler}]).

start_client(Url) ->
  supervisor:start_child(yberpc_sup, [{client, Url}]).

request(Pid, ReqData) ->
  gen_server:call(Pid, {request, ReqData}).

reply(Pid, RepData) ->
  gen_server:call(Pid, {reply, RepData}).

stop_server(Pid) ->
  supervisor:terminate_child(yberpc_sup, Pid).

stop_client(Pid) ->
  supervisor:terminate_child(yberpc_sup, Pid).

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
init([{server, Url, Handler}]) ->
  lager:debug("start_server: ~p", [Url]),
  process_flag(trap_exit, true),
  case enm:rep([{bind, Url}, {nodelay, true}]) of
    {ok, Sock} ->
      lager:debug("server pid: ~p, sock: ~p", [self(), Sock]),
      lager:debug("rpc_dbg: link it to self"),
      link(Sock),
      {ok, #state{sock = Sock, handler = Handler}};
    Else ->
      lager:error("enm:rep error: ~p", [Else]),
      {error, Else}
  end;

init([{client, Url}]) ->
  lager:debug("start_client: ~p", [Url]),
  process_flag(trap_exit, true),
  case enm:req([{connect, Url}, {nodelay, true}]) of
    {ok, Sock} ->
      lager:debug("client pid: ~p, sock: ~p", [self(), Sock]),
      {ok, #state{sock = Sock}};
    Else ->
      lager:error("enm:req: ~p", [Else]),
      {error, Else}
  end.

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

handle_call({request, ReqData}, _From, #state{sock = Sock} = State) ->
  lager:debug("rpc_dbg: form client, request ~p ~p, data: ~p", [self(), Sock, ReqData]),
  case send_data(Sock, ReqData) of
    ok ->
      receive
        {nnreq, Sock, Data} ->
              lager:debug("rpc_dbg: form client, request ~p ~p, data: ~p is responsed :~p", [self(), Sock, ReqData, Data]),
          {reply, {ok, Data}, State}
          %% let's remove timeout to see what will happen
%      after
%        ?RPC_TIMEOUT ->
%          lager:error("wait reply timeout"),
%          {reply, {error, timeout}, State}
      end;
    Else ->
      lager:error("send_data ~p", [Else]),
      {reply, Else, State}
  end;


%% this is a fake reply api
handle_call({reply, RepData}, _From, #state{sock = Sock} = State) ->
%  lager:debug("rpc_dbg: from server, now reply ~p ~p, data: ~p", [self(), Sock, RepData]),
%  Result = send_data(Sock, RepData),
%  case Result of
%      ok ->
%          lager:debug("rpc_dbg: from server, now reply ~p ~p, data: ~p, result is:~p", [self(), Sock, RepData, Result]);
%      ELSE ->
%          lager:debug("rpc_dbg: from server side, rpc reply fail ~p, return result is ~p", [RepData, Result])
%  end,
  {reply, ok , State};

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

handle_info({nnrep, Sock, Data}, #state{sock = Sock, handler = Handler} = State) ->
  lager:debug("receive a nnrep: ~p ~p", [self(), Sock]),
  Handler ! {yberpc_notify_req, {self(), Data}},
  Result = send_data(Sock, <<"ok">>),
  {noreply, State};

handle_info({'EXIT',Socket, Reason}, State) ->
  lager:debug("rpc_dbg Port driver is stopped, now shutdown, reason is:~p", [Reason]),
  {stop, Reason, State};

handle_info(Info, State) ->
  lager:debug("rpc_dbg: receive unknown message:~p", [Info]),
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
terminate(Reason, #state{sock = Sock} = State) ->
  lager:debug("rpc_dbg: close sock: ~p for reason:~p, and the whole state is : ~p", [Sock, Reason, State]),
  enm:close(Sock),
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
send_data(Sock, Data) ->
  case enm:send(Sock, Data) of
    ok ->
      ok;
    Else ->
      lager:error("enm:send error: sock: ~p, error: ~p", [Sock, Else]),
      Else
  end.
