# msgbus_rpc_proxy

Erlang RPC 库。

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



