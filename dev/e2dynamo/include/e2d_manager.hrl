-include("e2d_common.hrl").
-include_lib("kernel/include/inet.hrl").

%% the node info in the manager
-record(node_info, {
    id = 0 :: non_neg_integer(),
    name = '' :: atom(),
    ip = {0, 0, 0, 0} :: ip_address()
    }).

