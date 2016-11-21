-module(msgbus_rpc_proxy).

-behaviour(supervisor).

%% API
-export([start_link/0,
  start_server/2,
  start_client/2,
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

start_server(Url, Handler) ->
  supervisor:start_child(?MODULE, [{server, Url, Handler}]).

start_client(Url, Handler) ->
  supervisor:start_child(?MODULE, [{client, Url, Handler}]).

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
  {ok, {{simple_one_for_one, 1, 5},
    [{undefined, {msgbus_rpc_proxy_gs, start_link, []},
      transient, 5000, worker, [msgbus_rpc_proxy]}]}}.
