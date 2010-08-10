%% sysmon client_api模块的配置文件
-define(SYSMON_HOST, "localhost:8001").
-define(HTTP_TIMEOUT, 10000).


%%
%% 注意:
%% 通过Pattern查找进程的pid
%% pgrep -f 'Pattern'
%%
-define(REDIS_PATTERN, "redis-server").
-define(MYSQL_PATTERN, "mysqld").
-define(DISPATCHSERVER_PATTERN, "dispatchserver").
-define(EJABBERD_PATTERN, "ejabberd").
