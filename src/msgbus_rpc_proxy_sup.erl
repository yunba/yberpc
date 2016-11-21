-module(msgbus_rpc_proxy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
  start_server/1,
  start_client/1,
  set_handler/2,
  rpc/2,
  stop_server/1,
  stop_client/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_server(Url) ->
  supervisor:start_child(?MODULE, [{server, Url}]).

start_client(Url) ->
  supervisor:start_child(?MODULE, [{client, Url}]).

set_handler(Pid, Handler) ->
  gen_server:call(Pid, {set_handler, Handler}).

rpc(Pid, Data) ->
  gen_server:call(Pid, {rpc, Data}).

stop_server(Pid) ->
  supervisor:terminate_child(?MODULE, Pid).

stop_client(Pid) ->
  supervisor:terminate_child(?MODULE, Pid).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, {{simple_one_for_one, 0, 1},
    [{client, {msgbus_rpc_proxy, start_link, []},
      temporary, 5000, worker, [msgbus_rpc_proxy]}]}}.

