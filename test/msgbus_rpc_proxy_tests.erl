%%%-------------------------------------------------------------------
%%% @author shdxiang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Nov 2016 5:24 PM
%%%-------------------------------------------------------------------
-module(msgbus_rpc_proxy_tests).
-author("shdxiang").

-include_lib("eunit/include/eunit.hrl").

-compile({parse_transform, lager_transform}).

init_test() ->
  enm:start_link(),
  msgbus_rpc_proxy_sup:start_link(),
  ets:new(data, [set, public, named_table]).

start_server_test() ->
  {ok, Sock} = msgbus_rpc_proxy:start_server("tcp://*:9000", self()),
  ets:insert(data, {server_sock, Sock}).

start_client_test() ->
  {ok, Sock} = msgbus_rpc_proxy:start_client("tcp://localhost:9000", self()),
  ets:insert(data, {client_sock, Sock}).

rpc_test() ->
  [{client_sock, ClientSock}] = ets:lookup(data, client_sock),
  [{server_sock, ServerSock}] = ets:lookup(data, server_sock),

  Data = <<"test">>,
  msgbus_rpc_proxy:rpc(ClientSock, Data),
  receive
    {rpc_proxy_req, {ServerSock, Data}} ->
      receive
        {rpc_proxy_rep, {ClientSock, <<0>>}} -> ok
      after
        100 -> ?assert(false)
      end
  after
    100 -> ?assert(false)
  end.

stop_server_test() ->
  [{server_sock, ServerSock}] = ets:lookup(data, server_sock),
  ok = msgbus_rpc_proxy:stop_server(ServerSock).

stop_client_test() ->
  [{client_sock, ClientSock}] = ets:lookup(data, client_sock),
  ok = msgbus_rpc_proxy:stop_client(ClientSock).

rpc_one_time(ServerSock, ClientSock, Data) ->
  msgbus_rpc_proxy:rpc(ClientSock, Data),
  receive
    {rpc_proxy_req, {ServerSock, Data}} ->
      receive
        {rpc_proxy_rep, {ClientSock, <<0>>}} -> ok
      after
        100 -> ?assert(false)
      end
  after
    100 -> ?assert(false)
  end.

rpc_times(N, ServerSock, ClientSock, Data) when N > 0 ->
  rpc_one_time(ServerSock, ClientSock, Data),
  rpc_times(N - 1, ServerSock, ClientSock, Data);

rpc_times(N, _ServerSock, _ClientSock, _Data) when N =:= 0 ->
  ok.

build_buffer(Length) when Length > 1 ->
  Buf = build_buffer(Length - 1),
  <<<<"0">>/binary, Buf/binary>>;

build_buffer(Length) when Length =:= 1 ->
  <<"0">>.

benchmark_test_() ->
  {timeout, 60,
    fun() ->
      ?debugFmt("benchmark_test may take 10 seconds", []),
      N = 100000,
      DataLen = 1024,

      {ok, ServerSock} = msgbus_rpc_proxy:start_server("tcp://*:9000", self()),
      timer:sleep(100),
      {ok, ClientSock} = msgbus_rpc_proxy:start_client("tcp://localhost:9000", self()),
      timer:sleep(100),

      Data = build_buffer(DataLen),
      Begin = os:timestamp(),
      rpc_times(N, ServerSock, ClientSock, Data),
      End = os:timestamp(),
      Diff = timer:now_diff(End, Begin),

      ?debugFmt("data: ~p bytes, count: ~p, time: ~p ms", [DataLen, N, Diff / 1000]),
      msgbus_rpc_proxy:stop_client(ClientSock),
      msgbus_rpc_proxy:stop_server(ServerSock)
    end}.
