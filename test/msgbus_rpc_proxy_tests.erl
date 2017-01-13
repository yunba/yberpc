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
  msgbus_rpc_proxy:start_link(),
  ets:new(data, [set, public, named_table]).

start_server_test() ->
  {ok, Pid} = msgbus_rpc_proxy:start_server("tcp://*:9000", self()),
  ets:insert(data, {server_pid, Pid}).

start_client_test() ->
  {ok, Pid} = msgbus_rpc_proxy:start_client("tcp://localhost:9000", self()),
  ets:insert(data, {client_pid, Pid}).

rpc_test() ->
  [{client_pid, ClientPid}] = ets:lookup(data, client_pid),

  Data = <<"test">>,
  {ok, <<0>>} = msgbus_rpc_proxy:rpc(ClientPid, Data),
  receive
    {rpc_proxy_data, Data} -> ok
  after
    100 -> ?assert(false)
  end.

stop_server_test() ->
  [{server_pid, ServerPid}] = ets:lookup(data, server_pid),
  ok = msgbus_rpc_proxy:stop_server(ServerPid).

stop_client_test() ->
  [{client_pid, ClientPid}] = ets:lookup(data, client_pid),
  ok = msgbus_rpc_proxy:stop_client(ClientPid).

rpc_one_time(ClientPid, Data) ->
%%  timer:sleep(1),
  {ok, <<0>>} = msgbus_rpc_proxy:rpc(ClientPid, Data).

rpc_times(N, ClientPid, Data) when N > 0 ->
  rpc_one_time(ClientPid, Data),
  rpc_times(N - 1, ClientPid, Data);

rpc_times(N, _ClientPid, _Data) when N =:= 0 ->
  ok.

build_buffer(Length) when Length > 1 ->
  Buf = build_buffer(Length - 1),
  <<<<"0">>/binary, Buf/binary>>;

build_buffer(Length) when Length =:= 1 ->
  <<"0">>.

recv_one_time() ->
  receive
    {rpc_proxy_data, _} ->
      Result = case ets:lookup(data, count) of
                 [{count, Count}] ->
                   Count + 1;
                 _ ->
                   1
               end,
      ets:insert(data, {count, Result})
  end.

recv_times(N) when N > 0 ->
  recv_one_time(),
  recv_times(N - 1);

recv_times(N) when N =:= 0 ->
  ok.

benchmark_test_() ->
  {timeout, 60,
    fun() ->
      ?debugFmt("benchmark_test may take 10 seconds", []),
      N = 100000,
      DataLen = 4096,
      Receiver = spawn(fun() ->
        recv_times(N) end),

      {ok, ServerPid} = msgbus_rpc_proxy:start_server("tcp://*:9000", Receiver),
      timer:sleep(100),
      {ok, ClientPid} = msgbus_rpc_proxy:start_client("tcp://localhost:9000", self()),
      timer:sleep(100),

      Data = build_buffer(DataLen),
      Begin = os:timestamp(),
      rpc_times(N, ClientPid, Data),
      End = os:timestamp(),
      Diff = timer:now_diff(End, Begin),

      timer:sleep(1000),
%%      make sure server receive N message
      [{count, N}] = ets:lookup(data, count),

      ?debugFmt("data: ~p bytes, count: ~p, time: ~p ms", [DataLen, N, Diff / 1000]),
      msgbus_rpc_proxy:stop_client(ClientPid),
      msgbus_rpc_proxy:stop_server(ServerPid)
    end}.
