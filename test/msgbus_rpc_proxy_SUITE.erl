%%%-------------------------------------------------------------------
%%% @author shdxiang
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Jan 2017 2:01 PM
%%%-------------------------------------------------------------------
-module(msgbus_rpc_proxy_SUITE).
-author("shdxiang").

-compile({parse_transform, lager_transform}).

-compile(export_all).

-include("ct.hrl").

all() ->
  [rpc_test, benchmark_test].

init_per_suite(Config) ->
  ct:pal("init_per_suite"),
  {ok, _} = application:ensure_all_started(msgbus_rpc_proxy),
  Config.

end_per_suite(Config) ->
  ct:pal("end_per_suite"),
  ok = application:stop(msgbus_rpc_proxy),
  Config.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, Config) ->
  Config.

%% ===================================================================
%% Tests
%% ===================================================================
rpc_test(_Config) ->
  {ok, ServerPid} = msgbus_rpc_proxy:start_server("tcp://*:9000", self()),
  {ok, ClientPid} = msgbus_rpc_proxy:start_client("tcp://localhost:9000", self()),

  ReqData = <<"ReqData">>,
  RepData = <<"RepData">>,
  msgbus_rpc_proxy:rpc_req(ClientPid, ReqData),
  receive
    {rpc_proxy_rep, {ServerPid, ReqData}} ->
      msgbus_rpc_proxy:rpc_rep(ServerPid, RepData),
      receive
        {rpc_proxy_req, {ClientPid, RepData}} -> ok
      after
        100 ->
          ct:fail(unexpected)
      end
  after
    100 ->
      ct:fail(unexpected)
  end,
  ok = msgbus_rpc_proxy:stop_client(ClientPid),
  ok = msgbus_rpc_proxy:stop_server(ServerPid).

rpc_one_time(ServerPid, ClientPid, ReqData, RepData) ->
  msgbus_rpc_proxy:rpc_req(ClientPid, ReqData),
  receive
    {rpc_proxy_rep, {ServerPid, ReqData}} ->
      msgbus_rpc_proxy:rpc_rep(ServerPid, RepData),
      receive
        {rpc_proxy_req, {ClientPid, RepData}} -> ok
      after
        100 ->
          ct:fail(unexpected)
      end
  after
    100 ->
      ct:fail(unexpected)
  end.

rpc_times(N, ServerPid, ClientPid, ReqData, RepData) when N > 0 ->
  rpc_one_time(ServerPid, ClientPid, ReqData, RepData),
  rpc_times(N - 1, ServerPid, ClientPid, ReqData, RepData);

rpc_times(N, _ServerPid, _ClientPid, _ReqData, _RepData) when N =:= 0 ->
  ok.

build_buffer(Length) when Length > 1 ->
  Buf = build_buffer(Length - 1),
  <<<<"0">>/binary, Buf/binary>>;

build_buffer(Length) when Length =:= 1 ->
  <<"0">>.

benchmark_test(_Config) ->
  N = 100000,
  DataLen = 16,

  {ok, ServerPid} = msgbus_rpc_proxy:start_server("tcp://*:9000", self()),
  {ok, ClientPid} = msgbus_rpc_proxy:start_client("tcp://localhost:9000", self()),

  Data = build_buffer(DataLen),
  Begin = os:timestamp(),
  rpc_times(N, ServerPid, ClientPid, Data, Data),
  End = os:timestamp(),
  Diff = timer:now_diff(End, Begin),

  ct:pal("data: ~p bytes, count: ~p, time: ~p ms", [DataLen, N, Diff / 1000]),
  ok = msgbus_rpc_proxy:stop_client(ClientPid),
  ok = msgbus_rpc_proxy:stop_server(ServerPid).
