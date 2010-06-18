-include_lib("kernel/include/inet.hrl").
-include("vclock.hrl").

%%% type define

-type key() :: non_neg_integer().
-type value() :: binary().

-type bucket_id() :: non_neg_integer().

-type context() :: vclock().
-type request() :: {'get', key()} | {'put', vclock()} | {'del', key()}.

-type node_id() :: non_neg_integer().
-type node_list() :: [node_id()].

-type node_status() :: 'init' | 'running' | 'down'.

%% record defines
-record(e2dnode, {
          id = 0 :: node_id(),
          name = '' :: atom(),
          ip = {0, 0, 0, 0} :: ip_address(),
          port = 0 :: ip_port(),
          capacity = 0 :: pos_integer()
         }).

-type e2dnode() :: #e2dnode{}.
