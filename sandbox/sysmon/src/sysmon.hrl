-record(monitor_app, {app :: atom(),     %% E.g. 'ejabberd', 'dispatchserver'       
		      host :: string(),  %% Host IP  "xxx.xxx.xxx.xxx"
		      type :: atom(),    %% E.g. 'cpu', 'memory'
		      time :: any(),     %% 记录产生的时间
		      value :: any()}).  %% 记录数据

-record(check_alive, {app :: atom(),         %% E.g. 'ejabberd', 'dispatchserver'       
                      host :: string(),      %% Host IP  "xxx.xxx.xxx.xxx"
                      time :: any(),         %% 记录产生的时间
                      value :: boolean()}).  %% 服务是否在运行, true表示正在运行, false表示停止


-record(auto_service, {app :: atom(),         %% E.g. 'ejabberd', 'dispatchserver'       
                       host :: string(),      %% Host IP  "xxx.xxx.xxx.xxx"
                       time :: any(),         %% 记录产生的时间
                       type :: boolean(),     %% 日志类型, true表示正确, false表示错误
                       value :: string()}).   %% 日志的具体内容
