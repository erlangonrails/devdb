-define(PORT, 11211).
-define(MAX_PACKET_SIZE, 300000).
-define(FSMOPTS, [{debug, [trace]}]).

-record(storage_command, {key, flags, exptime, bytes, data=""}).
