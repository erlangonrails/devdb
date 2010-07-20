%% ---------------------------------
%% Logging mechanism

%% Print in standard output
-define(PRINT(Format, Args),
    io:format(Format, Args)).

-define(DEBUG(Format, Args),
    ds_logger:debug_msg(?MODULE,?LINE,Format, Args)).

-define(INFO_MSG(Format, Args),
    ds_logger:info_msg(?MODULE,?LINE,Format, Args)).
			      
-define(WARNING_MSG(Format, Args),
    ds_logger:warning_msg(?MODULE,?LINE,Format, Args)).
			      
-define(ERROR_MSG(Format, Args),
    ds_logger:error_msg(?MODULE,?LINE,Format, Args)).

-define(CRITICAL_MSG(Format, Args),
    ds_logger:critical_msg(?MODULE,?LINE,Format, Args)).
