-module(sysmon_google_chart_api).
-export([build_url_monitor_app/3]).

-define(GOOGLE_CHART_API_PREFIX, "http://chart.apis.google.com/chart").
-define(GOOGLE_CHART_API_CHLS, "chls=1,2,0"). %% 折线样式
-define(GOOGLE_CHART_API_CHCO, "chco=339933"). %% 数据颜色
-define(GOOGLE_CHART_API_CHF, "chf=bg,s,FEFFEA"). %% 背景颜色
-define(GOOGLE_CHART_API_CHT, "cht=lc").
-define(GOOGLE_CHART_API_CHS, "chs=600x300").
-define(GOOGLE_CHART_API_CHXT, "chxt=x,y").

%% ========================================================================
%% http://chart.apis.google.com/chart? \
%%     chf=bg,s,efefef& \
%%     chls=1,1,0& \
%%     chco=00ff00& \
%%     cht=lc& \
%%     chs=600x300& \
%%     chd=t:1,2,4,3,4,100,5,100,200,0,0,0& \
%%     chxt=x,y& \
%%     chxl=0:|x-1|x-2|x-3|x-4|x-5|1:||y-max+(MB)
%%
%% 说明:
%% http://chart.apis.google.com/chart? Chart API 调用地址
%% &参数分隔符
%% chs=600x300图表尺寸
%% chd=t:1,2,4,3,4,100,5,100,200,0,0,0图表数据值
%% cht=lc图表类型
%% chxt=x,y显示 x、y 轴坐标
%% chxl=0:|x-1|x-2|x-3|x-4|x-5|1:||y-max+(MB)x、y 轴坐标值
%%
%% 细节:
%% 1. chd=t:文本编码格式为
%% chd=t:<数值字符串>
%% 其中<数值字符串>为0.0至100.0的浮点数、-1（负一）和|（竖线）.
%%
%% 含义如下
%% 浮点数字0.0= 0, 以此类推100.0= 100
%% 负一-1表示空值
%% 2. chls=
%% <数据线 1 粗细>,<数据线 1 线段长度>,<数据线 1 线段间隔长度>
%% ==========================================================================
-spec build_url_monitor_app(DataList :: [any()], XList :: [string()], YMax :: string()) -> string().
build_url_monitor_app(DataList, XList, YMax) ->
    Chd = build_chd(DataList),
    Chxl = build_chxl(XList, YMax),
    Ret = io_lib:format("~s?~s&~s&~s&~s&~s&~s&~s&~s",
		        [?GOOGLE_CHART_API_PREFIX,
			 ?GOOGLE_CHART_API_CHF,
			 ?GOOGLE_CHART_API_CHLS,
			 ?GOOGLE_CHART_API_CHCO,
			 ?GOOGLE_CHART_API_CHT,
			 ?GOOGLE_CHART_API_CHS,
			 Chd,
			 ?GOOGLE_CHART_API_CHXT,
			 Chxl]),
    lists:flatten(Ret).

%% ==========================================
%% Internal APIs:
%% ==========================================

build_chd(DataList) ->
    Ret = lists:foldl(fun(Item, AccIn) ->
	                 case AccIn of
			     "chd=t:" ->
			         io_lib:format("~s~p", [AccIn, Item]);
			      _ ->
				 io_lib:format("~s,~p", [AccIn, Item])
                         end
                     end, "chd=t:", DataList),
    lists:flatten(Ret).

build_chxl(XList, YMax) ->
    Ret = lists:foldl(fun(Item, AccIn) ->
		         case AccIn of
			     "chxl=0:" ->
				 io_lib:format("~s|~s|", [AccIn, Item]);
			     _ ->
				 io_lib:format("~s~s|", [AccIn, Item])
                         end
                     end, "chxl=0:", XList),
    lists:flatten(Ret ++ "1:||" ++ YMax).
    
    
