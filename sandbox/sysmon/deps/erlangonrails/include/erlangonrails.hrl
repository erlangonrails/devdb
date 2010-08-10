%% Session Id key
-define(ERAILS_SID, "_erails_session_id_"). %% Cookie中的Sid的Key

%% Environment data

-define(ERAILS_ENV_DATA, [{method, "REQUEST_METHOD"},               %% 'GET', 'POST'
			  {content_type, "CONTENT_TYPE"},           
			  {content_length,"CONTENT_LENGTH"},        
			  {path_info, "PATH_INFO"},                 %% 请求的URL路径
			  {remote_addr, "REMOTE_ADDR"},             %% 客户端IP
			  {cookie, "COOKIES"},                      %% Cookie:proplists()
			  {erails_params, "ERAILS_DATA"}]).         %% POST或者GET过来的数据 
