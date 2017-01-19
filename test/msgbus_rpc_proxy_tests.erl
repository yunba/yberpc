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
  {ok, Pid} = msgbus_rpc_proxy:start_server("tcp://*:9000", self()),
  ets:insert(data, {server_pid, Pid}).

start_client_test() ->
  {ok, Pid} = msgbus_rpc_proxy:start_client("tcp://localhost:9000", self()),
  ets:insert(data, {client_pid, Pid}).

rpc_test() ->
  [{client_pid, ClientPid}] = ets:lookup(data, client_pid),
  [{server_pid, ServerPid}] = ets:lookup(data, server_pid),

  Data = <<"test">>,
  msgbus_rpc_proxy:rpc(ClientPid, Data),
  receive
    {rpc_proxy_req, {ServerPid, Data}} ->
      receive
        {rpc_proxy_rep, {ClientPid, <<0>>}} -> ok
      after
        100 -> ?assert(false)
      end
  after
    100 -> ?assert(false)
  end.

stop_server_test() ->
  [{server_pid, ServerPid}] = ets:lookup(data, server_pid),
  ok = msgbus_rpc_proxy:stop_server(ServerPid).

stop_client_test() ->
  [{client_pid, ClientPid}] = ets:lookup(data, client_pid),
  ok = msgbus_rpc_proxy:stop_client(ClientPid).

rpc_one_time(ServerPid, ClientPid, Data) ->
  msgbus_rpc_proxy:rpc(ClientPid, Data),
  receive
    {rpc_proxy_req, {ServerPid, Data}} ->
      receive
        {rpc_proxy_rep, {ClientPid, <<0>>}} -> ok
      after
        100 -> ?assert(false)
      end
  after
    100 -> ?assert(false)
  end.

rpc_times(N, ServerPid, ClientPid, Data) when N > 0 ->
  rpc_one_time(ServerPid, ClientPid, Data),
  rpc_times(N - 1, ServerPid, ClientPid, Data);

rpc_times(N, _ServerPid, _ClientPid, _Data) when N =:= 0 ->
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

      {ok, ServerPid} = msgbus_rpc_proxy:start_server("tcp://*:9000", self()),
      timer:sleep(100),
      {ok, ClientPid} = msgbus_rpc_proxy:start_client("tcp://localhost:9000", self()),
      timer:sleep(100),

      Data = build_buffer(DataLen),
      Begin = os:timestamp(),
      rpc_times(N, ServerPid, ClientPid, Data),
      End = os:timestamp(),
      Diff = timer:now_diff(End, Begin),

      ?debugFmt("data: ~p bytes, count: ~p, time: ~p ms", [DataLen, N, Diff / 1000]),
      msgbus_rpc_proxy:stop_client(ClientPid),
      msgbus_rpc_proxy:stop_server(ServerPid)
    end}.
