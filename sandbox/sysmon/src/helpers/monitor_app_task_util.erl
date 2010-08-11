-module(monitor_app_task_util).
-include("../sysmon.hrl").

-define(SRV_MONITOR_APP, "monitor_app").
-export([gen_filename/1, gen_image_url/1, collect_result/1]).

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
		    YMax1 = io_lib:format("~p+(%CPU)", [YMax]),
                    sysmon_google_chart_api:build_url_monitor_app(DataList, XList, YMax1);
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
cal_delta_y(0) ->
    10 div 100;
cal_delta_y(0.0) ->
    10.0 / 100;
cal_delta_y(0.00) ->
    10.00 / 100;
cal_delta_y(Data) when is_integer(Data) ->
    Delta = Data div 10,
    (Delta + Data) div 100;
cal_delta_y(Data) when is_float(Data) ->
    Delta = Data / 10,
    (Delta + Data) / 100.
