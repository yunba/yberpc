%%%-------------------------------------------------------------------
%%% @author shdxiang
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Feb 2017 6:19 PM
%%%-------------------------------------------------------------------
-module(yberpc_adapter).
-author("shdxiang").

-behaviour(gen_server).

-compile({parse_transform, lager_transform}).

%% API
-export([start_link/0,
  start_servers/1,
  stop_servers/0,
  set_clients/2,
  request/2,
  request_by_id/3]).

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
  servers = []
}).

%%%===================================================================
%%% API
%%%===================================================================
start_servers(Handler) ->
  lager:debug("start_servers"),
  gen_server:call(?MODULE, {start_servers, Handler}).

stop_servers() ->
  lager:debug("stop_servers"),
  gen_server:call(?MODULE, stop_servers).

set_clients(Key, Values) ->
  lager:debug("set_clients: ~p ~p", [Key, Values]),
  gen_server:call(?MODULE, {set_clients, {Key, Values}}).

request(Key, Data) ->
  lager:debug("request: ~p", [Key]),
  gen_server:call(?MODULE, {request, {Key, Data}}).

request_by_id(Key, Id, Data) ->
  lager:debug("request_by_id: ~p ~p", [Key, Id]),
  gen_server:call(?MODULE, {request_by_id, {Key, Id, Data}}).

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
  ets:new(yberpc_adapter_client, [set, named_table, private]),
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

handle_call({start_servers, Handler}, _From, State) ->
  Servers = do_start_servers(Handler),
  {reply, ok, State#state{servers = Servers}};

handle_call(stop_servers, _From, #state{servers = Servers} = State) ->
  do_stop_servers(Servers),
  {reply, ok, State#state{servers = []}};

handle_call({set_clients, {Key, Values}}, _From, State) ->
  do_set_clients(Key, Values),
  {reply, ok, State};

handle_call({request, {Key, Data}}, _From, State) ->
  Result = do_request(Key, Data),
  {reply, Result, State};

handle_call({request_by_id, {Key, Id, Data}}, _From, State) ->
  Result = do_request_by_id(Key, Id, Data),
  {reply, Result, State};

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
do_start_servers(Handler) ->
  Servers = application:get_env(yberpc, servers, []),
  lists:map(fun(Server) ->
    {Url} = get_server_info(Server),
    {ok, Pid} = yberpc:start_server(Url, Handler),
    {Pid} end, Servers).

do_stop_servers(Servers) ->
  lists:map(fun(Server) ->
    {Pid} = Server,
    yberpc:stop_server(Pid) end, Servers).

do_set_clients(Key, StringValues) ->
  {ok, Values} = parse_values(StringValues),
  NewClients =
    case ets:lookup(yberpc_adapter_client, Key) of
      [{_, Clients}] ->
        Clients2 =
          lists:filtermap(fun(Client) ->
            {Location, Id, _, Pid} = Client,
            case lists:keyfind(Location, 1, Values) of
              {_, Weight} ->
                {true, {Location, Id, Weight, Pid}};
              _ ->
                yberpc:stop_client(Pid),
                false
            end end, Clients),
        Clients3 =
          lists:filtermap(fun(Value) ->
            {Location, Id, Weight} = Value,
            case lists:keyfind(Location, 1, Clients2) of
              false ->
                case yberpc:start_client(Location, self()) of
                  {ok, Pid} ->
                    {true, {Location, Id, Weight, Pid}};
                  Else ->
                    lager:error("yberpc:start_client: ~p", [Else]),
                    false
                end;
              _ ->
                false
            end end, Values),
        lists:append(Clients2, Clients3);
      _ ->
        lists:filtermap(fun(Value) ->
          {Location, Id, Weight} = Value,
          case yberpc:start_client(Location, self()) of
            {ok, Pid} ->
              {true, {Location, Id, Weight, Pid}};
            Else ->
              lager:error("yberpc:start_client: ~p", [Else]),
              false
          end end, Values)
    end,
  ets:insert(yberpc_adapter_client, {Key, NewClients}).

do_request(Key, Data) ->
  case ets:lookup(yberpc_adapter_client, Key) of
    [{_, Clients}] ->
      clients_request(Clients, Data);
    _ ->
      lager:error("no clients for: ~p", [Key]),
      {error, no_client}
  end.

do_request_by_id(Key, Id, Data) ->
  case ets:lookup(yberpc_adapter_client, Key) of
    [{_, Clients}] ->
      Clients2 =
        lists:filter(fun(Client) ->
          case Client of
            {_, Id, _, _} ->
              true;
            _ ->
              false
          end end, Clients),
      clients_request(Clients2, Data);
    _ ->
      lager:error("no clients for: ~p", [Key]),
      {error, no_client}
  end.

get_server_info(Server) ->
  {server, Info} = Server,
  Url = proplists:get_value(url, Info),
  {Url}.

clients_request([], _Data) ->
  lager:error("all clients failed"),
  {error, all_failed};

clients_request(Clients, Data) ->
  Client = select_one_client(Clients),
  case clients_request_one(Client, Data) of
    {ok, RepData} ->
      {ok, RepData};
    Else ->
      lager:error("clients_request_one: ~p", [Else]),
      Clients2 = lists:delete(Client, Clients),
      clients_request(Clients2, Data)
  end.

select_one_client(Clients) ->
  lists:nth(random:uniform(length(Clients)), Clients).

clients_request_one(Client, ReqData) ->
  {_, _, _, Pid} = Client,
  case yberpc:rpc_req(Pid, ReqData) of
    ok ->
      wait_reply(Pid);
    Else ->
      lager:error("yberpc:rpc_req: ~p", [Else]),
      Else
  end.

parse_values(StringValues) ->
  Values = lists:map(fun(Value) ->
    {Decoded} = jiffy:decode(Value),
    Location = proplists:get_value(<<"location">>, Decoded),
    Id = proplists:get_value(<<"id">>, Decoded),
    Weight = proplists:get_value(<<"weight">>, Decoded),
    {Location, Id, Weight} end, StringValues),
  lager:debug("~p", [Values]),
  {ok, Values}.

wait_reply(Pid) ->
  receive
    {yberpc_notify_rep, {Pid, RepData}} ->
      {ok, RepData}
  after
    ?RPC_TIMEOUT ->
      {error, timeout}
  end.