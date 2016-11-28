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

%%simple_test() ->
%%  ?assert(true).

init_test() ->
  enm:start_link(),
  msgbus_rpc_proxy:start_link(),
  ets:new(data, [set, named_table]).

start_server_test() ->
  {ok, Pid} = msgbus_rpc_proxy:start_server("tcp://*:9000", self()),
  ets:insert(data, {server_pid, Pid}).

start_client_test() ->
  {ok, Pid} = msgbus_rpc_proxy:start_client("tcp://localhost:9000", self()),
  ets:insert(data, {client_pid, Pid}).

rpc_test() ->
  [{client_pid, ClientPid}] = ets:lookup(data, client_pid),

  Data = <<"test">>,
  ok = msgbus_rpc_proxy:rpc(ClientPid, Data),
  receive
    {rpc_proxy_data, Data} -> ignore
  after
    100 -> ?assert(false)
  end.

stop_server_test() ->
  [{server_pid, ServerPid}] = ets:lookup(data, server_pid),
  ok = msgbus_rpc_proxy:stop_server(ServerPid).

stop_client_test() ->
  [{client_pid, ClientPid}] = ets:lookup(data, client_pid),
  ok = msgbus_rpc_proxy:stop_client(ClientPid).
