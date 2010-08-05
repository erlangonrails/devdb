-module(erails_datetime).
-export([get/0, 
         get_date/0,
         get_time/0,
         get_str/0,
         get_date_str/0,
         get_time_str/0]).

%% Eraong On Rails中时间类型的定义
%%
%% @type date() = {Year, Month, Day}
%% @type time() = {Hour, Minute, Second}
%% @type datetime() = {date, time}
%% @type Year, Month, Day, Hour, Minute, Second = integer()


-spec(get() -> {{integer(), integer(), integer()},{integer(), integer(), integer()}}).
get() ->
    calendar:local_time().

-spec(get_date() -> {integer(), integer(), integer()}).
get_date() ->
    erlang:date().

-spec(get_time() -> {integer(), integer(), integer()}).
get_time() ->
    erlang:now().

-spec(get_str() -> string()).
get_str() ->
    {{Y, Month, D},{H, Minute, S}} = calendar:local_time(),
    integer_to_list(Y) ++ integer_to_list(Month) ++ integer_to_list(D) 
      ++ integer_to_list(H) ++ integer_to_list(Minute) ++ integer_to_list(S).

-spec(get_date_str() -> string()).
get_date_str() ->
    {Y, M, D} = erlang:date(),
    integer_to_list(Y) ++ integer_to_list(M) ++ integer_to_list(D).  

-spec(get_time_str() -> string()).
get_time_str() ->
    {H, M, S} = erlang:now(),
    integer_to_list(H) ++ integer_to_list(M) ++ integer_to_list(S).
