-module(erails_env).
-include("erlangonrails.hrl").
-export([setup/1]).

%% 在后续版本中可能考虑用ewgi替代该模块:
%% http://code.google.com/p/ewgi/

%%
%% 对Req中的数据进行解析, 返回一个proplists(), 这个proplists()就是这次HTTP请求对应的Env,
%% 这个返回的Env可以被erails_var模块使用.
%%

%% ================================================
%% External APIs:
%% ================================================

%%
%% @doc
%% 解析Req, 返回一个proplists().
-spec(setup(any()) -> [{any(), any()}]).
setup(Req) ->
    [{"req_ori", Req} | parse_request(Req)].

%% ================================================
%% Internal APIs:
%% ================================================

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
    
			       
