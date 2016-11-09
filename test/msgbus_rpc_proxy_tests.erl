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

simple_test() ->
  ?assert(true).

init_test() ->
  lager:start(),
  ets:new(sock_table, [named_table, protected, set]).

start_link_test() ->
  {ok, _State} = msgbus_rpc_proxy:start_link().

create_server_test() ->
  {ok, SockServer} = msgbus_rpc_proxy:create_server("tcp://127.0.0.1:9010"),
  ets:insert(sock_table, {sock_server, SockServer}).

create_client_test() ->
  {ok, SockClient} = msgbus_rpc_proxy:create_client("tcp://127.0.0.1:9010"),
  ets:insert(sock_table, {sock_client, SockClient}).

sync_request_test() ->
  [{_, SockClient}] = ets:lookup(sock_table, sock_client),
  msgbus_rpc_proxy:sync_request(SockClient, <<"data">>).
