-define(CRON_ANY,   1). %% "*"
-define(CRON_NUM,   2). %% 2
-define(CRON_RANGE, 4). %% 2-3
-define(CRON_LIST,  8). %% "2,3-6"
-type cronf_type() :: 1 | 2 | 4 | 8.

-type cronf_num() :: non_neg_integer().
-type cronf_range() :: {non_neg_integer(), non_neg_integer(), pos_integer()}.
-type cronf_list() :: [cronf_num() | cronf_range()].

-type cronf_value() :: cronf_num() | cronf_range() | cronf_list().

-record(cron_field, {
        type = ?CRON_ANY :: cronf_type(),   % field type
        value :: cronf_value()              % field value
    }).
-type cron_field() :: #cron_field{}.

-record(cron_entry, {
        m :: cron_field(),       % minute
        h :: cron_field(),       % hour
        dom :: cron_field(),     % day of month
        mon :: cron_field(),     % month
        dow :: cron_field(),     % day of week
        mfa :: tuple()           % the mfa
   }).
-type cron_entry() :: #cron_entry{}.


%% some log defines
-define(Debug(F, D), (io:format(F, D))).
-define(Warn(F, D), (io:format(F, D))).
-define(Error(F, D), (io:format(F, D))).
