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
-define(VALUES, [<<"{\"location\":\"tcp://localhost:9000\",\"id\":\"test_id\",\"weight\":100}">>]).

all() ->
  [
    rpc_test,
    benchmark_test,
    adapter_test_request,
    adapter_test_request_by_id,
    client_selector
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
  {ok, ClientPid} = yberpc:start_client(?URL),

  ReqData = <<"ReqData">>,
  RepData = <<"RepData">>,
  ok = yberpc:request(ClientPid, ReqData, self()),
  receive
    {yberpc_notify_req, {ServerPid, ReqData}} ->
      yberpc:reply(ServerPid, RepData),
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
  {ok, ClientPid} = yberpc:start_client(?URL),

  Data = build_buffer(DataLen),
  Begin = os:timestamp(),
  rpc_times(N, ServerPid, ClientPid, Data, Data),
  End = os:timestamp(),
  Diff = timer:now_diff(End, Begin),

  ct:pal("data: ~p bytes, count: ~p, time: ~p ms", [DataLen, N, Diff / 1000]),
  ok = yberpc:stop_client(ClientPid),
  ok = yberpc:stop_server(ServerPid).

adapter_test_request(_Config) ->
  spawn(fun() ->
    yberpc_adapter:start_servers(self()),
    receive
      {yberpc_notify_req, {ServerPid, ReqData}} ->
        RepData = handle_data(ReqData),
        yberpc:reply(ServerPid, RepData)
    end end),

  timer:sleep(200),
  yberpc_adapter:set_clients(?KEY, ?VALUES),
  {ok, <<"123456">>} = yberpc_adapter:request(?KEY, <<"654321">>),
  yberpc_adapter:stop_servers().

adapter_test_request_by_id(_Config) ->
  spawn(fun() ->
    yberpc_adapter:start_servers(self()),
    receive
      {yberpc_notify_req, {ServerPid, ReqData}} ->
        RepData = handle_data(ReqData),
        yberpc:reply(ServerPid, RepData)
    end end),

  timer:sleep(200),
  yberpc_adapter:set_clients(?KEY, ?VALUES),
  {ok, <<"123456">>} = yberpc_adapter:request_by_id(?KEY, <<"test_id">>, <<"654321">>),
  yberpc_adapter:stop_servers().

rpc_one_time(ServerPid, ClientPid, ReqData, RepData) ->
  ok = yberpc:request(ClientPid, ReqData, self()),
  receive
    {yberpc_notify_req, {ServerPid, ReqData}} ->
      yberpc:reply(ServerPid, RepData),
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
  ct:pal("RepData: ~p", [RepData]),
  RepData.

client_selector(_) ->
    Client1 = {"localhost:1100", "hello", 100, 10},
    Client2 = {"localhost:999", "hello", 0, 10},
    Client3 = {"localhost:999", "hello", 20, 10},
    Clients = [Client1, Client2],
    %% one is 100 weith, and 2 is 0 weight, so it must return 1
    Client1 = yberpc_client_selector:select_one(Clients),

    %% only Client2 is inputed, so return Client2
    Client2 = yberpc_client_selector:select_one([Client2]),

    %% return undefined when empty list is used
    undefined = yberpc_client_selector:select_one([]),
    ok.
