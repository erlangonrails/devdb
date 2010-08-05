-module(erails_env).
-include("erlangonrails.hrl").
-export([setup/1]).

%% 在后续版本中可能考虑用ewgi替代该模块:
%% http://code.google.com/p/ewgi/

%% 如果我们访问http://127.0.0.1:8000/user/show, 经过setup/1以后,
%% 可能获得如下类似的Evn:proplists().
%%
%% 对Req中的数据进行解析, 返回一个proplists(), 这个proplists()就是这次HTTP请求对应的Env,
%% 这个返回的Env可以被erails_var模块使用.
%%
%% [{"REMOTE_ADDR","127.0.0.1"},
%%  {"PATH_INFO","/user/show"},
%%  {"REQUEST_METHOD",'GET'},
%%  {"SERVER_PORT","8000"},
%%  {"SERVER_PROTOCOL","HTTP/1.1"},
%%  {"SERVER_NAME","127.0.0.1"},
%%  {"SERVER_SOFTWARE","MochiWeb"},
%%  {"HTTP_USER_AGENT",
%%   "Mozilla/5.0 (X11; U; Linux i686; zh-CN; rv:1.9.1.3) Gecko/20091020 Ubuntu/9.10 (karmic) Firefox/3.5.3"},
%%  {"HTTP_KEEP_ALIVE","300"},
%%  {"HTTP_HOST","127.0.0.1:8000"},
%%  {"HTTP_CONNECTION","keep-alive"},
%%  {"HTTP_ACCEPT_LANGUAGE","zh-cn,zh;q=0.5"},
%%  {"HTTP_ACCEPT_ENCODING","gzip,deflate"},
%%  {"HTTP_ACCEPT_CHARSET","GB2312,utf-8;q=0.7,*;q=0.7"},
%%  {"HTTP_ACCEPT","text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"}]


%% External APIs:

%%
%% @doc
%% 解析Req, 返回一个proplists().
-spec(setup(any()) -> [{any(), any()}]).
setup(Req) ->
    parse_request(Req).


%% Internal APIs:
nhdr(L) when is_atom(L) ->
    nhdr(atom_to_list(L));
nhdr(L) when is_binary(L) ->
    nhdr(binary_to_list(L));
nhdr(L) when is_list(L) ->
    underscoreize(L, []).

%% 将所有的$-转化成$_
underscoreize([], S) ->
    lists:reverse(S);
underscoreize([$-|T], S) ->
    underscoreize(T, [$_|S]);
underscoreize([H|T], S) ->
    underscoreize(T, [H|S]).

%% 作两个格式化操作: upper和strip
normalize_header({K, V}) ->
    {string:to_upper(string:strip(nhdr(K))), string:strip(V)}.

parse_request(Req) ->
    Hdrs = parse_headers(Req),
    lists:foldl(fun({El, ElName}, PList) ->
                        V = parse_element(El, Req),
                        case V of
                            undefined -> PList;
                            V ->
                                NewEl = proplists:property({ElName, V}),
                                [NewEl|PList]
                        end
                end, Hdrs, ?ERAILS_ENV_DATA).

parse_element(server_sw, _Req) ->
    "MochiWeb";
parse_element(server_name, Req) ->
    HostPort = Req:get_header_value(host),
    case HostPort of
        HostPort when is_list(HostPort) ->
            hd(string:tokens(HostPort, ":"));
        HostPort -> HostPort
    end;
parse_element(server_port, Req) ->
    HostPort = Req:get_header_value(host),
    case HostPort of
        HostPort when is_list(HostPort) ->
	    TokensHostPort = string:tokens(HostPort, ":"),
            case length(TokensHostPort) of
                2 -> lists:nth(2, TokensHostPort);
                _ -> undefined
            end;
        _ ->
            undefined
    end;
parse_element(server_protocol, Req) ->
    {Maj, Min} = Req:get(version),
    lists:flatten(io_lib:format("HTTP/~b.~b", [Maj, Min]));
parse_element(method, Req) ->
    Req:get(method);
parse_element(path_info,Req) ->
    Req:get(path);
parse_element(remote_addr, Req) ->
    Req:get(peer);
parse_element(cookie, Req) ->
    %% parse_cookie() -> [{Key::string(), Value::string()}] 
    Req:parse_cookie();
parse_element(erails_params,Req) ->
    case Req:get(method) of
	%% 获取POST或者GET的数据, 返回的是一个[{Key:string(), Value:string()}]
	Method when Method =:= 'GET'; Method =:= 'HEAD' ->
	    Req:parse_qs(); 
	_ ->
	    Req:parse_post()
    end;
parse_element(content_type, Req) ->
    Req:get_header_value("content-type");
parse_element(content_length, Req) ->
    case Req:get_header_value("content-length") of
        undefined -> undefined;
        Length when is_integer(Length) ->
            Length;
        Length when is_list(Length) ->
            list_to_integer(Length)
    end.

%% 所有从headers中解析出的数据都以HTTP_开头.
parse_headers(Req) ->
    Hdrs = Req:get(headers),
    lists:foldl(fun(Pair, Acc) ->
                        {K1, V1} = normalize_header(Pair),
			%% 我们已经在parse_element/2的时候解析出了content-length和content-type
                        case K1 of
                            "CONTENT_LENGTH" ->
                                Acc;
                            "CONTENT_TYPE" ->
                                Acc;
                            K1 ->
                                [{lists:append(["HTTP_", K1]), V1}|Acc]
                        end
                end,
                [],
                mochiweb_headers:to_list(Hdrs)).
    
			       
