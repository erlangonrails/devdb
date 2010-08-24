%% sysmon client_api模块的配置文件
-define(SYSMON_HOST, "localhost:8001"). %% sysmon server服务的地址
-define(HTTP_TIMEOUT, 10000).           %% HTTP请求的timeout
-define(LOCALHOST_DEF, "127.0.0.1").    %% 本地的IP, 这个会发送给服务器端


%%
%% 目前支持的application
%%
%% 注意:
%% 通过Pattern查找进程的pid
%% pgrep -f 'Pattern'
%%
-define(REDIS_PATTERN, "redis-server").
-define(MYSQL_PATTERN, "/usr/sbin/mysqld").
-define(DISPATCHSERVER_PATTERN, "dispatchserver").
-define(EJABBERD_PATTERN, "ejabberd").
-define(MURMUR_PATTERN, "murmurd").

%% application的名字
-define(REDIS_APP, 'redis').
-define(MYSQL_APP, 'mysql').
-define(DISPATCHSERVER_APP, 'dispatchserver').
-define(EJABBERD_APP, 'ejabberd').
-define(MURMUR_APP, 'murmur').

%%
%% application启动的命令行配置
%%
-define(REDIS_START_CMD, "/usr/local/redis/bin/redis-server /usr/local/redis/etc/redis.conf").
-define(MYSQL_START_CMD, "/etc/init.d/mysql start").
-define(DISPATCHSERVER_START_CMD, "/usr/local/dispatchserver/bin/dispatchserverctl start").
-define(EJABBERD_START_CMD, "/usr/local/imserver/sbin/ejabberdctl start").
-define(MURMUR_START_CMD, "/usr/local/murmur/murmurd &").

%%
%% 对于非原生的application, 如ejabberd, dispatchserver, 我们使用check command来检测进程是否存活 :)
%%
-define(REDIS_CHECK_CMD, undefined).
-define(MYSQL_CHECK_CMD, undefined).
-define(DISPATCHSERVER_CHECK_CMD, "/usr/local/dispatchserver/bin/dispatchserverctl status").
-define(EJABBERD_CHECK_CMD, "/usr/local/imserver/sbin/ejabberdctl status").
-define(MURMUR_CHECK_CMD, undefined).

%% check command的返回值
-define(REDIS_CHECK_CMD_RET, undefined).
-define(MYSQL_CHECK_CMD_RET, undefined).
-define(EJABBERD_CHECK_CMD_RET, "ejabberd 2.1.4 is running in that node").
-define(DISPATCHSERVER_CHECK_CMD_RET, "dispatchserver 0.1 is running in that node").
-define(MURMUR_CHECK_CMD_RET, undefined).
