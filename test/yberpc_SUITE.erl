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
    benchmark_test_for_connections,
    adapter_test_request,
    adapter_test_request_multiple_clients,
    adapter_test_request_by_id,
    client_selector,
    one_process_stop_when_another_process_is_sending,
    rpc_parse_value
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
  spawn(fun() ->
    {ok, ServerPid} = yberpc:start_server(?URL, self()),
    receive
      {yberpc_notify_req, {ServerPid, ReqData}} ->
        RepData = handle_data(ReqData),
        yberpc:reply(ServerPid, RepData)
    end,
    ok = yberpc:stop_server(ServerPid) end),

  timer:sleep(100),
  {ok, ClientPid} = yberpc:start_client(?URL),
  {ok, <<"123456">>} = yberpc:request(ClientPid, <<"654321">>),
  ok = yberpc:stop_client(ClientPid),
  timer:sleep(100).

adapter_test_request_multiple_clients(_config) ->
  N = 10000,
  DataLen = 64,
  Data = build_buffer(DataLen),

  Server = spawn(fun() ->
    {ok, ServerPid} = yberpc:start_server(?URL, self()),
    handle_request(Data),
    ok = yberpc:stop_server(ServerPid) end),
  {ok, Client1Pid} = yberpc:start_client(?URL),
  {ok, Client2Pid} = yberpc:start_client(?URL),
  CurPid = self(),
  spawn(fun() -> 
      rpc_times(N, Client1Pid, Data, Data),
      CurPid ! one, 
      ok = yberpc:stop_client(Client1Pid) 
  end),
  spawn(fun() -> 
      rpc_times(N, Client2Pid, Data, Data),
      CurPid ! two,
      ok = yberpc:stop_client(Client2Pid) 
  end),

  wait_one_two_done([]),
  Server ! server_finish,
  timer:sleep(100),

  ok.

wait_one_two_done([one, two]) ->
    ok;
wait_one_two_done([two, one]) ->
    ok;
wait_one_two_done(Done) ->
    receive
        X -> wait_one_two_done([X|Done])
    end.

one_process_stop_when_another_process_is_sending(_config) ->
  N = 10000,
  DataLen = 64,
  Data = build_buffer(DataLen),

  Server = spawn(fun() ->
    {ok, ServerPid} = yberpc:start_server(?URL, self()),
    handle_request(Data),
    ok = yberpc:stop_server(ServerPid) end),

  {ok, ClientPid} = yberpc:start_client(?URL),
  {ok, Client2Pid} = yberpc:start_client(?URL),
  Client = spawn(fun() ->
      try
          rpc_times(N, ClientPid, Data, Data)
      catch
          _:_ -> ct:pal("catch exception")
      end
  end),

  timer:sleep(100),
  %% stop the client if even if the client is sending
  ok = yberpc:stop_client(ClientPid),

  {ok, AnotherClientPid} = yberpc:start_client(?URL),
  rpc_times(N, AnotherClientPid, Data, Data),

  Server ! server_finish,
  timer:sleep(100),

  ok.

benchmark_test(_Config) ->
  N = 10000,
  DataLen = 64,
  Data = build_buffer(DataLen),

  Server = spawn(fun() ->
    {ok, ServerPid} = yberpc:start_server(?URL, self()),
    handle_request(Data),
    ok = yberpc:stop_server(ServerPid) end),

  timer:sleep(100),
  {ok, ClientPid} = yberpc:start_client(?URL),

  Begin = os:timestamp(),
  rpc_times(N, ClientPid, Data, Data),
  End = os:timestamp(),
  Diff = timer:now_diff(End, Begin),
  ok = yberpc:stop_client(ClientPid),
  Server ! server_finish,
  timer:sleep(100),

  ct:pal("data: ~p bytes, count: ~p, time: ~p ms", [DataLen, N, Diff / 1000]).

benchmark_test_for_connections(_Config) ->
  N = 10000,
  DataLen = 64,
  Data = build_buffer(DataLen),

  Server = spawn(fun() ->
    {ok, ServerPid} = yberpc:start_server(?URL, self()),
    handle_request(Data),
    ok = yberpc:stop_server(ServerPid) end),

  timer:sleep(100),
  SpinningClients = start_five_hundred_clients(),

  {ok, ClientPid} = yberpc:start_client(?URL),

  Begin = os:timestamp(),
  rpc_times(N, ClientPid, Data, Data),
  End = os:timestamp(),
  Diff = timer:now_diff(End, Begin),
  ok = yberpc:stop_client(ClientPid),

  lists:foreach(fun(Client) -> ok = yberpc:stop_client(Client) end, SpinningClients),

  Server ! server_finish,
  timer:sleep(100),

  ct:pal("data: ~p bytes, count: ~p, time: ~p ms", [DataLen, N, Diff / 1000]).

start_five_hundred_clients() ->
    start_five_hundred_clients(0, []).

start_five_hundred_clients(200, Started) ->
    Started;
start_five_hundred_clients(Counter, Started) ->
  ct:log("counter :~p", [Counter]),
  {ok, ClientPid} = yberpc:start_client(?URL),
  start_five_hundred_clients(Counter + 1, [ClientPid | Started]).

adapter_test_request(_Config) ->
  spawn(fun() ->
    yberpc_adapter:start_servers(self()),
    receive
      {yberpc_notify_req, {ServerPid, ReqData}} ->
        RepData = handle_data(ReqData),
        yberpc:reply(ServerPid, RepData)
    end,
    yberpc_adapter:stop_servers() end),

  timer:sleep(100),
  yberpc_adapter:set_clients(?KEY, ?VALUES),
  {ok, <<"123456">>} = yberpc_adapter:request(?KEY, <<"654321">>),
  timer:sleep(100).

adapter_test_request_by_id(_Config) ->
  spawn(fun() ->
    yberpc_adapter:start_servers(self()),
    receive
      {yberpc_notify_req, {ServerPid, ReqData}} ->
        RepData = handle_data(ReqData),
        yberpc:reply(ServerPid, RepData)
    end,
    yberpc_adapter:stop_servers() end),

  timer:sleep(100),
  yberpc_adapter:set_clients(?KEY, ?VALUES),
  {ok, <<"123456">>} = yberpc_adapter:request_by_id(?KEY, <<"test_id">>, <<"654321">>),
  timer:sleep(100).

rpc_one_time(ClientPid, ReqData, RepData) ->
  {ok, RepData} = yberpc:request(ClientPid, ReqData).

rpc_times(N, ClientPid, ReqData, RepData) when N > 0 ->
  rpc_one_time(ClientPid, ReqData, RepData),
  rpc_times(N - 1, ClientPid, ReqData, RepData);

rpc_times(N, _ClientPid, _ReqData, _RepData) when N =:= 0 ->
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
  Clients = [Client1, Client2],
  %% one is 100 weith, and 2 is 0 weight, so it must return 1
  Client1 = yberpc_client_selector:select_one(Clients),

  %% only Client2 is inputed, so return Client2
  Client2 = yberpc_client_selector:select_one([Client2]),

  %% return undefined when empty list is used
  undefined = yberpc_client_selector:select_one([]),
  ok.

handle_request(Data) ->
  receive
    {yberpc_notify_req, {ServerPid, Data}} ->
      yberpc:reply(ServerPid, Data),
      handle_request(Data);
    server_finish ->
      ok
  end.

rpc_parse_value(_Config) ->
    String1 = "{\"location\" : \"192.169.0.1\", \"id\": \"hello\", \"weight\":11}",
    String2 = "{\"location\" : \"192.169.0.1\",  \"weight\":11}",

    {ok, [{<<"192.169.0.1">>, <<"hello">>, 11}] } = yberpc_adapter:parse_values([ String1 ]),
    %% id is alternative
    {ok, [{<<"192.169.0.1">>, undefined, 11}] } = yberpc_adapter:parse_values([ String2 ]).

