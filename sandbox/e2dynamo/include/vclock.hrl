%%% vector header
-type vc_node() :: any().
-type vc_stamp() :: pos_integer().

-record(vc_entry, {
                                        node = <<>> :: vc_node(),
                                        counter = 0 :: non_neg_integer(),
                                        stamp = 0 :: vc_stamp()
                                        }).

-type vc_entry() :: #vc_entry{}.

-type vclock() :: [vc_entry()].
