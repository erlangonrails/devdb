-record(monitor_app, {app :: atom(),     %% E.g. 'ejabberd', 'dispatchserver'       
		      host :: string(),  %% Host IP  "xxx.xxx.xxx.xxx"
		      type :: atom(),    %% E.g. 'cpu', 'memory'
		      time :: any(),     %% 记录产生的时间
		      value :: any()}).  %% 记录数据
