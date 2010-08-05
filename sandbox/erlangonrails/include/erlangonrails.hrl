%% Session Id key
-define(ERAILS_SID, "_erails_session_id_").

%% Environment data:
%% if we access http://127.0.0.1:8000/user/show
%%
%% [{"REMOTE_ADDR","127.0.0.1"},
%%  {"PATH_INFO","/user/show"},
%%  {"REQUEST_METHOD",'GET'},
%%  {"SERVER_PORT","8000"},
%%  {"SERVER_PROTOCOL","HTTP/1.1"},
%%  {"SERVER_NAME","127.0.0.1"},
%%  {"SERVER_SOFTWARE","MochiWeb"},

-define(ERAILS_ENV_DATA, [{server_sw, "SERVER_SOFTWARE"},
			  {server_name, "SERVER_NAME"},
			  {server_protocol, "SERVER_PROTOCOL"},
			  {server_port, "SERVER_PORT"},
			  {method, "REQUEST_METHOD"},
			  {content_type, "CONTENT_TYPE"},
			  {content_length,"CONTENT_LENGTH"},
			  {path_info, "PATH_INFO"},
			  {remote_addr, "REMOTE_ADDR"},
			  {cookie, "COOKIES"},
			  {erails_params, "erails.data"}]). %% POST或者GET的数据 
