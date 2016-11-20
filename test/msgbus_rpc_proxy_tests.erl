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
  {ok, _State} = msgbus_rpc_proxy:start_link(),
  ets:new(data, [set, named_table]).

start_server_test() ->
  {ok, Sock} = msgbus_rpc_proxy:start_server("tcp://*:9000"),
  ets:insert(data, {server_sock, Sock}).

start_client_test() ->
  {ok, Sock} = msgbus_rpc_proxy:start_client("tcp://localhost:9000"),
  ets:insert(data, {client_sock, Sock}).

join_handler_test() ->
  ok = msgbus_rpc_proxy:join_handler(self()),
  [{client_sock, ClientSock}] = ets:lookup(data, client_sock),

  Data = <<"test">>,
  ok = msgbus_rpc_proxy:rpc(ClientSock, Data),
  receive
    {rpc_proxy_data, _ServerSock, Data} -> ignore
  after
    100 -> ?assert(false)
  end.

leave_handler_test() ->
  ok = msgbus_rpc_proxy:leave_handler(self()),
  [{client_sock, ClientSock}] = ets:lookup(data, client_sock),

  Data = <<"test">>,
  ok = msgbus_rpc_proxy:rpc(ClientSock, Data),
  receive
    {rpc_proxy_data, _ServerSock, _Data} -> ?assert(false)
  after
    100 -> ignore
  end.

stop_server_test() ->
  [{server_sock, Sock}] = ets:lookup(data, server_sock),
  ok = msgbus_rpc_proxy:stop_server(Sock).

stop_client_test() ->
  [{client_sock, Sock}] = ets:lookup(data, client_sock),
  ok = msgbus_rpc_proxy:stop_client(Sock).
