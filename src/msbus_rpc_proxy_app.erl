-module(msbus_rpc_proxy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    msbus_rpc_proxy_sup:start_link().

stop(_State) ->
    ok.
