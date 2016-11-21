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
  {ok, _Pid} = enm:start_link(),
  msgbus_rpc_proxy_sup:start_link(),
  ets:new(data, [set, named_table]).

start_server_test() ->
  {ok, Pid} = msgbus_rpc_proxy_sup:start_server("tcp://*:9000"),
  ets:insert(data, {server_pid, Pid}).

start_client_test() ->
  {ok, Pid} = msgbus_rpc_proxy_sup:start_client("tcp://localhost:9000"),
  ets:insert(data, {client_pid, Pid}).

set_handler_test() ->
  [{server_pid, ServerPid}] = ets:lookup(data, server_pid),
  [{client_pid, ClientPid}] = ets:lookup(data, client_pid),

  ok = msgbus_rpc_proxy_sup:set_handler(ServerPid, self()),

  Data = <<"test">>,
  ok = msgbus_rpc_proxy_sup:rpc(ClientPid, Data),
  receive
    {rpc_proxy_data, Data} -> ignore
  after
    100 -> ?assert(false)
  end.

stop_server_test() ->
  [{server_pid, ServerPid}] = ets:lookup(data, server_pid),
  ok = msgbus_rpc_proxy_sup:stop_server(ServerPid).

stop_client_test() ->
  [{client_pid, ClientPid}] = ets:lookup(data, client_pid),
  ok = msgbus_rpc_proxy_sup:stop_server(ClientPid).
