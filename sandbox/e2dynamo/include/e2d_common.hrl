% access info
-define(Log(F, D), error_logger:info_report(io_lib:format(F, D))).

% error
-define(Error(F, D), error_logger:error_report(io_lib:format(F, D))).

% warnning
-define(Warn(F, D), error_logger:warning_report(io_lib:format(F, D))).

% debug info
-define(Debug(F, D), io:format(F, D)).
%-define(Debug(F, D), ok).

-define(NOT_IMPL, exit(not_impl)).

-define(debug, true).

-ifdef(debug).
-include_lib("eunit/include/eunit.hrl").
-endif.
