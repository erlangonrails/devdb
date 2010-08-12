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

-define(REDIS_APP, 'redis').
-define(MYSQL_APP, 'mysql').
-define(DISPATCHSERVER_APP, 'dispatchserver').
-define(EJABBERD_APP, 'ejabberd').
