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
  {ok, _State} = msgbus_rpc_proxy:start_link().

start_server_test() ->
  {ok, _Sock} = msgbus_rpc_proxy:start_server("tcp://*:9010").

start_client_test() ->
  {ok, _Sock} = msgbus_rpc_proxy:start_client("tcp://localhost:9010").
