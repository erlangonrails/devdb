-module(erails_guid).
-export([get/1,
	 get_with_datetime/1,
	 get_with_date/1,
	 get_with_time/1]).

%% Erlang On Rails中GUID的生成模块
%%
%% 注意: 这个模块以来crypto模块, 在使用之前必须调用crypto:start/0.
%%

%% @spec get(Len:integer()) -> string()
-spec(get(integer()) -> string()).
get(Len) ->
    Chars =
	{$a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n,$o,$p,$q,$r,$s,$t,$u,$v,
	 $w,$x,$y,$z,
	 $1,$2,$3,$4,$5,$6,$7,$8,$9,$0},
    Res = lists:map(
	    fun(_) ->
		    Idx = crypto:rand_uniform(1, 36),
		    element(Idx, Chars)
	    end, lists:seq(1, Len)),
    Res.


-spec(get_with_datetime(integer()) -> string()).
get_with_datetime(SuffixLen) ->
   erails_datetime:get_str() ++ erails_guid:get(SuffixLen).


-spec(get_with_date(integer()) -> string()).
get_with_date(SuffixLen) ->
    erails_datetime:get_date_str() ++ erails_guid:get(SuffixLen).


-spec(get_with_time(integer()) -> string()).
get_with_time(SuffixLen) ->
    erails_datetime:get_time_str() ++ erails_guid:get(SuffixLen).
