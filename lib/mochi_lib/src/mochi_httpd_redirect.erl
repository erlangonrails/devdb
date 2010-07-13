-module(mochi_httpd_redirect).
-export([start/0, start/1, stop/0, handle_request/2]).

%% Default config
-define(MOCHI_IP, "0.0.0.0").
-define(MOCHI_PORT, 8004).
-define(MOCHI_DOCROOT, ".").
-define(MOCHI_NAME, ?MODULE).

%% 演示Mochiweb输出动态内容和静态文件的能力:
%% 
%% 测试:
%% http://localhost:8004/login - 访问这个URL, 会自动跳转到/user/liqiang这个URL
%%

%% External API
start() ->
    start([]).

start(Options) ->
    Loop = fun(Req) -> 
	       ?MODULE:handle_request(Req, get_option(docroot, Options, ?MOCHI_DOCROOT)) 
	   end,
    mochiweb_http:start([{loop, Loop},
			 {port, get_option(port, Options, ?MOCHI_PORT)},
			 {ip, get_option(ip, Options, ?MOCHI_IP)},
			 {name, get_option(name, Options, ?MOCHI_NAME)}]).

stop() ->
    mochiweb_http:stop(?MODULE).


handle_request(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    Method = Req:get(method),
    {Main, Minor} = Req:get(version),
    log(Method, Path, Main, Minor),
    case Method of
	'GET' ->
	    route_req_get(Req, Path, DocRoot);
	'HEAD' ->
	    route_req_get(Req, Path, DocRoot);
	'POST' ->
	    route_req_post(Req, Path, DocRoot);
	_ ->
            Req:respond({500,[],"mochi_httpd error"})
    end.

%% Internal API
route_req_get(Req, "login", _DocRoot) ->
    Req:respond({302, [{"Location","/user/liqiang"}], ""});
route_req_get(Req, "user/liqiang", _DocRoot) ->
    Req:respond({200, [{"Content-Type","text/plain"}], "this is /usr/liqiang page!!!"});
route_req_get(Req, _Path, _DocRoot) ->
    Req:respond({200, [{"Content-Type","text/plain"}], "route_req_get"}).

route_req_post(Req, _Path, _DocRoot) ->
    Req:respond({200, [{"Content-Type","text/plain"}], "route_req_post"}).


get_option(Key, Options, Default) ->
    case proplists:get_value(Key, Options) of
	undefined ->
	    Default;
	Val ->
	    Val
    end.

log(Method, Path, Main, Minor) ->
    io:format("~s ~s HTTP ~B.~B~n", [Method, Path, Main, Minor]).
