-define(FSMOPTS, [{debug, [trace]}]).

-record(storage_command, {key, flags, exptime, bytes, data=""}).
