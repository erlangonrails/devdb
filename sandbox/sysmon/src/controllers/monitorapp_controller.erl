-module(monitorapp_controller,[Env]).

-export([handle_request/2,before_filter/0]).

handle_request("index",[]) ->
    {render,"monitorapp/index.html",[{data, "monitorapp"}, {items, []}]};
handle_request(App,[]) ->
    Items = get_items(App),
    {render,"monitorapp/index.html",[{data, "monitorapp-" ++ App}, {items, Items}]};
handle_request(App, [Type, Host, Time]) ->
    Filename = "monitor_app#" ++ App ++ "#" ++ Type ++ "#" ++ Host ++ "#" ++ Time ++ ".data",
    {MaxVal, MinVal} = monitor_app_task_util:get_max_min_value(Filename),
    ImgUrl = monitor_app_task_util:gen_image_url(Filename),
    SourceData = get_sourcedata(Filename),
    {render,"monitorapp/reports.html",[{data, "monitorapp-details-" ++ App}, 
	                               {filename, Filename}, 
	                               {imgurl, ImgUrl},
	                               {maxval, io_lib:format("(~p)", [MaxVal])},
	                               {minval, io_lib:format("(~p)", [MinVal])},
	                               {sourcedata, SourceData}]}.

before_filter() ->
    FilterOnly = ["index", "ejabberd", "redis", "mysql", "dispatchserver"],
    case lists:member(erails_var:get_action(Env), FilterOnly) of
	true ->
	    case sysmon_acl:is_login(Env) of
		true ->
		    ok;
		false -> %% 用户没有登录, 重定向到登录页面 :)
		    {redirect, "/login"}
            end;
	false ->
	    {text, "no action!!!"}
    end.


%% 处理Items, 增加一个tag
get_items(MyApp) ->
    Items = monitor_app_task_util:get_sorted_files_by_app(MyApp),
    {_, Ret} = lists:foldl(fun({"monitor_app", App, Type, Ip, NewTime}, {Tag, AccIn}) ->
                              ResKey = App ++ "#" ++ Type ++ "#" ++ Ip,
                              ResVal = "<a href=/monitorapp/"++ App ++ "/" ++ Type ++ "/" ++ Ip ++ "/" ++ NewTime ++">"++ "点击查看详细日志:"++ NewTime ++"</a>",
                              {not Tag, [{not Tag, ResKey, ResVal}|AccIn]}
                          end, {true, []}, Items),
    lists:reverse(Ret).

%% {monitor_app,dispatchserver,"127.0.0.1",cpu,{{2010,8,12},{11,5,14}},0.0}.
get_sourcedata(Filename) ->
    {ok, Items} = sysmon_util:consult(Filename),
    {_, Ret} = lists:foldl(fun({MonitorAppAtom, _AppAtom, _Host, _TypeAtom, _Time, _Value}=Record, {Tag, AccIn}) ->
                              ResKey = atom_to_list(MonitorAppAtom),
                              ResVal = io_lib:format("~p", [Record]),
                              {not Tag, [{not Tag, ResKey, ResVal}|AccIn]}
                          end, {true, []}, Items),
    lists:reverse(Ret).
 
    

