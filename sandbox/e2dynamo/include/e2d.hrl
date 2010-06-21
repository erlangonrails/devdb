-include("e2d_common.hrl").
-include("e2d_type.hrl").

%% request timeout
-define(API_TIMEOUT, 1000).

%% the max value size(bytes) 1M
-define(MAX_VALUE_LEN, 16#100000).

%% Node Id length in bytes(2^32)
-define(NODE_ID_LEN, 4).

%% key Id length in bytes
-define(KEY_ID_LEN, 16).

%% key Id length in bits
-define(KEY_ID_BITS, 128).
