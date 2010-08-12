-module(monitor_app_task_util).
-include("../sysmon.hrl").

-define(SRV_MONITOR_APP, "monitor_app").
-export([get_sorted_files_by_app/1, get_files_by_app/1, sort/1, gen_filename/1, gen_image_url/1, collect_result/1, get_max_min_value/1]).


get_sorted_files_by_app(App) ->
    Files = get_files_by_app(App),
    SortFiles = monitor_app_task_util:sort(Files),
    Ret = lists:foldl(fun(Item, AccIn) ->
                          case string:tokens(Item, "#") of
	                      ["monitor_app", App, Type, Ip, Time] -> 
                                  NewTime = lists:sublist(Time, length(Time) - 5),
	                          [ {"monitor_app", App, Type, Ip, NewTime}| AccIn];
                              _ -> AccIn
                          end
                      end, [], SortFiles),
    lists:reverse(Ret).

%% @doc
%% 获取data文件夹下面app相关的所有数据文件.
-spec get_files_by_app(App :: string()) -> [string()].
get_files_by_app(App) ->
    {ok, Files} = sysmon_util:list_dir(),
    FilesFilter = lists:filter(fun(Item) ->
	                           case string:tokens(Item, "#") of
	                               ["monitor_app", App, _Type, _Ip, _Others] -> true;
                                       _ -> false
                                   end
                               end, Files),
    FilesFilter.

%% @doc
%% Note: .ignore file将导致该方法失败.
sort(Files) ->
    Ret = lists:sort(fun(File1, File2) ->
                         %% E.g. ["monitor_app","dispatchserver","cpu","127.0.0.1", "2010_8_12.data"]
                         ["monitor_app", _, _, _, Time1] = string:tokens(File1, "#"),
                         ["monitor_app", _, _, _, Time2] = string:tokens(File2, "#"),
                         NewTime1 = lists:sublist(Time1, length(Time1) - 5), %% remove ".data"
                         NewTime2 = lists:sublist(Time2, length(Time2) - 5),
                         [Y1, M1, D1] = string:tokens(NewTime1, "_"),
                         [Y2, M2, D2] = string:tokens(NewTime2, "_"),
                         case list_to_integer(Y1) > list_to_integer(Y2) of
	                     true -> true;
	                     false ->
                                 case list_to_integer(M1) > list_to_integer(M2) of
	                             true -> true;
                                     false ->
                                         case list_to_integer(D1) > list_to_integer(D2) of
	                                     true -> true;
	                                     false -> false
                                         end
                                 end
                         end
                     end, Files),
    Ret.

%% @doc
%% 根据#monitor_app{}, 产生它对应的文件名
-spec gen_filename(Term :: #monitor_app{}) -> string().
gen_filename(#monitor_app{app = App, 
                          host = Host,
			  type = Type,
			  time = Time,
			  value = _Value} = _Term) ->
    LogFile = io_lib:format("~s#~p#~p#~s#~s.data", 
			        [?SRV_MONITOR_APP, 
				 App, 
				 Type, 
				 Host, datetime_to_filename(Time)]),
    lists:flatten(LogFile).

-spec gen_image_url(Filename :: string()) -> string() | {error, any()}.
gen_image_url(Filename) ->
    case collect_result(Filename) of
	{error, Reason} ->
	    {error, Reason};
	{DataList, XList, YMax} ->
            [_MonitorApp, _App, Type, _Host, _Other] = string:tokens(Filename, "#"),
	    Type1 = list_to_atom(Type),
            case Type1 of
		'cpu' ->
                    %% Fix the format issue, 有的浮点数4.000000234特别难看, 把小数点后面的都去除掉:)
                    [YMaxStr] = io_lib:format("~p", [YMax]),
                    YMax1 = case string:tokens(YMaxStr, ".") of
	                      [A1, _A2] -> A1;
                              [A3] -> A3
                            end,
		    YMax2 = io_lib:format("~s+(%CPU)", [YMax1]),
                    sysmon_google_chart_api:build_url_monitor_app(DataList, XList, YMax2);
		'memory' ->
                    YMax1 = case YMax > 2048 of
				true ->
				    io_lib:format("~p+(MB)", [YMax div 1024]); %% convert to MB
				false ->
				    io_lib:format("~p+(KB)", [YMax])
                            end,
		    sysmon_google_chart_api:build_url_monitor_app(DataList, XList, YMax1)
            end;
	_Any ->
	    {error, unknown}
    end.


-spec collect_result(Filename :: string()) -> {error, any()} | {[any()], [string()], any()}.
collect_result(Filename) ->
    case sysmon_util:consult(Filename) of
	{ok, Items} ->
	    {Max, _Min} = get_max_min_value(Filename), %% TODO: fix {error, xxxx}
            Delta = cal_delta_y(Max),
            Ret = lists:foldl(fun(#monitor_app{value=ArgValue}, AccIn) ->
                                  if
				      is_integer(ArgValue) ->
					  [ArgValue div Delta | AccIn];
				      is_float(ArgValue) ->
					  [ArgValue / Delta | AccIn];
				      true ->
					  AccIn
                                  end
                              end, [], Items),

            %% 产生返回值 :)
            YMax = Delta * 100,
            #monitor_app{time=FirstTime} = hd(Items),
            #monitor_app{time=LastTime} = lists:last(Items),
            {lists:reverse(Ret), [datetime_to_x(FirstTime), datetime_to_x(LastTime)], YMax};
	{error, Reason} ->
	    {error, Reason}
    end.


%% Internal APIs:

datetime_to_filename({{Y,M,D},{_,_,_}}) ->
    integer_to_list(Y) ++ "_" ++ integer_to_list(M) ++ "_" ++ integer_to_list(D).

datetime_to_x({{_,_,_}, {H,M,S}}) ->
    integer_to_list(H) ++ "/" ++ integer_to_list(M) ++ "/" ++ integer_to_list(S).


%% @doc
%% 解析data文件, 找出其中最大值和最小值, 返回{Max, Min}.
-spec get_max_min_value(Filename :: string()) -> {error, any()} | {any(), any()}.
get_max_min_value(Filename) ->
    case sysmon_util:consult(Filename) of
	{ok, Items} ->
	    #monitor_app{value=FirstValue} = hd(Items),
            lists:foldl(fun(#monitor_app{value=ArgValue}, {CurMax, CurMin}) ->
			    ResMax = case CurMax < ArgValue of
					 true ->
					     ArgValue;
					 false ->
					     CurMax
                                     end,
                            ResMin = case CurMin > ArgValue of
					 true ->
					     ArgValue;
					 false ->
					     CurMin
                                     end,
			    {ResMax, ResMin}
                        end, {FirstValue, FirstValue}, Items);
	{error, Reason} ->
	    {error, Reason}
    end.

%% Fix the 0 issue.
cal_delta_y(0.0) ->
    10.0 / 100;
cal_delta_y(Data) when is_integer(Data) ->
    Delta = Data div 2,
    (Delta + Data) div 100;
cal_delta_y(Data) when is_float(Data) ->
    Delta = Data / 2,
    (Delta + Data) / 100.
