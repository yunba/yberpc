%%%-------------------------------------------------------------------
%%% @author shdxiang
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Jan 2017 2:01 PM
%%%-------------------------------------------------------------------
-module(yberpc_SUITE).
-author("shdxiang").

-compile({parse_transform, lager_transform}).

-compile(export_all).

-include("ct.hrl").

-define(URL, "tcp://127.0.0.1:9000").
-define(KEY, <<"key">>).
-define(VALUES, [{<<"tcp://127.0.0.1:9000">>, 50}]).

all() ->
  [
    rpc_test,
    benchmark_test,
    adapter_test
  ].

init_per_suite(Config) ->
  ct:pal("init_per_suite"),
  {ok, _} = application:ensure_all_started(yberpc),
  Config.

end_per_suite(Config) ->
  ct:pal("end_per_suite"),
  ok = application:stop(yberpc),
  Config.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, Config) ->
  Config.

%% ===================================================================
%% Tests
%% ===================================================================
rpc_test(_Config) ->
  {ok, ServerPid} = yberpc:start_server(?URL, self()),
  {ok, ClientPid} = yberpc:start_client(?URL, self()),

  ReqData = <<"ReqData">>,
  RepData = <<"RepData">>,
  ok = yberpc:rpc_req(ClientPid, ReqData),
  receive
    {yberpc_notify_req, {ServerPid, ReqData}} ->
      yberpc:rpc_rep(ServerPid, RepData),
      receive
        {yberpc_notify_rep, {ClientPid, RepData}} -> ok
      after
        100 ->
          ct:fail(unexpected)
      end
  after
    100 ->
      ct:fail(unexpected)
  end,
  ok = yberpc:stop_client(ClientPid),
  ok = yberpc:stop_server(ServerPid).

benchmark_test(_Config) ->
  N = 100000,
  DataLen = 64,

  {ok, ServerPid} = yberpc:start_server(?URL, self()),
  {ok, ClientPid} = yberpc:start_client(?URL, self()),

  Data = build_buffer(DataLen),
  Begin = os:timestamp(),
  rpc_times(N, ServerPid, ClientPid, Data, Data),
  End = os:timestamp(),
  Diff = timer:now_diff(End, Begin),

  ct:pal("data: ~p bytes, count: ~p, time: ~p ms", [DataLen, N, Diff / 1000]),
  ok = yberpc:stop_client(ClientPid),
  ok = yberpc:stop_server(ServerPid).

adapter_test(_Config) ->
  yberpc_adapter:start_servers(self()),

  yberpc_adapter:set_clients(?KEY, ?VALUES, self()),
  yberpc_adapter:request(?KEY, <<"654321">>),
  receive
    {yberpc_notify_req, {ServerPid, <<"654321">>}} ->
      yberpc:rpc_rep(ServerPid, <<"123456">>),
      receive
        {yberpc_notify_rep, {_ClientPid, <<"123456">>}} ->
          ok
      after
        100 ->
          ct:fail(unexpected)
      end
  after
    100 ->
      ct:fail(unexpected)
  end,
  ok = yberpc_adapter:stop_servers().

rpc_one_time(ServerPid, ClientPid, ReqData, RepData) ->
  ok = yberpc:rpc_req(ClientPid, ReqData),
  receive
    {yberpc_notify_req, {ServerPid, ReqData}} ->
      yberpc:rpc_rep(ServerPid, RepData),
      receive
        {yberpc_notify_rep, {ClientPid, RepData}} -> ok
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

handle_data(ReqData) ->
  ct:pal("ReqData: ~p", [ReqData]),
  List = binary_to_list(ReqData),
  List2 = lists:reverse(List),
  RepData = list_to_binary(List2),
  ct:pal("RepData: ~p", [RepData]).