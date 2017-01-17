# msgbus_rpc_proxy

Erlang RPC 库。

## API

## start_server(Url, Handler)

启动 RPC 服务器。_Url_ 为监听地址，_Handler_ 为处理收到数据的进程 `PID`，返回一个 socket。

## start_client(Url, Handler)

启动 RPC 客户端。_Url_ 为连接地址，_Handler_ 为处理收到数据的进程 `PID`，返回一个 socket，

## rpc(Sock, ReqData)

发起 RPC 调用。_Sock_ 为 `start_client` 返回的 socket，_ReqData_ 为请求数据。

## stop_server(Sock)

停止 RPC 服务器。_Sock_ 为 `start_server` 返回的 socket。

## stop_client(Sock)

停止 RPC 客户端。_Sock_ 为 `start_client` 返回的 socket。

## Benchmark

测试环境：

- OS: ubuntu 16.04 64-bit
- CPU: Intel(R) Core(TM) i7-6500U CPU @ 2.50GHz * 4
- Memory: 12G
- RPC 服务器和客户端都在此机器上

不同数据大小的 RPC 请求次数与耗时对应如下（`ms` 表示毫秒）：

**data: 1024 bytes：**

```
count: 50000, time: 640.131 ms
count: 100000, time: 1222.584 ms
count: 200000, time: 2300.181 ms
```

**data: 4096 bytes：**

```
count: 50000, time: 726.482 ms
count: 100000, time: 1456.664 ms
count: 200000, time: 2804.314 ms
```

**data: 16384 bytes：**

```
count: 50000, time: 913.373 ms
count: 100000, time: 1859.66 ms
count: 200000, time: 3727.51 ms
```
