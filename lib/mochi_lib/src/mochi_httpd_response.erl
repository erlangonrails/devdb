-module(mochi_httpd_response).
-export([start/0, start/1, stop/0, handle_request/2]).

%% Default config
-define(MOCHI_IP, "0.0.0.0").
-define(MOCHI_PORT, 8004).
-define(MOCHI_DOCROOT, ".").
-define(MOCHI_NAME, ?MODULE).

%% 演示Mochiweb输出动态内容和静态文件的能力:
%% 
%% 准备:
%% a. 准备一张test.jpg放置在MOCHI_DOCROOT目录下
%% 
%% 测试:
%% http://localhost:8004/text_plain - 显示text/plain内容
%% http://localhost:8004/text_html  - 显示text/html内容
%% http://localhost:8004/pic        - 显示静态文件
%% http://localhost:8004/image/1    - REST风格的图片显示API

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
route_req_get(Req, "text_plain", _DocRoot) ->
    Req:respond({200, [{"Content-Type","text/plain"}], "<html><body></body><body><h1>text_plain</h1></body></html>"});
route_req_get(Req, "text_html", _DocRoot) ->
    Req:respond({200, [{"Content-Type","text/html"}], "<html><body></body><body><h1>text_html</h1></body></html>"});
route_req_get(Req, "pic", _DocRoot) ->
    Req:respond({200, [{"Content-Type","text/html"}], "<html><body></body><body><h1>text_html</h1><img src='image/1'/></body></html>"});
route_req_get(Req, "image/1", DocRoot) ->   %% REST API :)
    Req:serve_file("test.jpg", DocRoot);
route_req_get(Req, _Path, _DocRoot) ->
    Req:respond({200, [{"Content-Type","text/plain"}], "route_req_post"}).

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
