
-define(PRINT(Format, Args), 
          io:format(Format, Args)).

-define(RET_SUCCESS, ok).
-define(RET_FAILED, error).

-define(VHOST, "91youbao.com").
-define(SERVER, "localhost").
-define(PORT, 5222).
