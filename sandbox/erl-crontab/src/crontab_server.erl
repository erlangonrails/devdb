-module(crontab_server).
-behaviour(gen_server).
-include("crontab.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {
        file = "" :: string(),          % file name
        mtime = 0 :: pos_integer(),     % last modify time
        entrys = [] :: [cron_entry()],  % the cron tasks
        file_timer :: reference(),      % the check file last modified timer
        cron_timer :: reference()       % the check cron task timer
    }).

-define(CRON_FILE, "crontab.cfg").
-define(CHECK_FILE_INTERVAL, 60000). % 1 min
-define(CHECK_CRON_INTERVAL, 60000). % 1 min
-define(SERVER, ?MODULE).
        
%% @doc start the cron server
-spec start_link() ->
    {'ok', pid()} | 'ignore' | {'error', any()}.
start_link() ->
    ?Debug("~p start_link~n", [?SERVER]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% gen_server callbacks
init(_Args) ->
    process_flag(trap_exit, true),
    case crontab_parse:parse(?CRON_FILE) of
        {ok, Entrys} ->
            ?Debug("parse the crontab success~n", []),
            State = #state{
                file = ?CRON_FILE,
                mtime = filelib:last_modified(?CRON_FILE),
                entrys = Entrys,
                file_timer = check_file_timer(),
                cron_timer = check_cron_timer()
            },
            {ok, State};
        Error ->
            ?Error("error :~p", [Error]),
            Error
    end.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _Ref, check_file}, State = #state{file = File, mtime = MTime}) ->
    State2 = State#state{
        file_timer = check_file_timer()
    },
    MTimeNew = filelib:last_modified(File),
    case  MTimeNew > MTime of
        true -> % reload crontab
            case crontab_parse:parse(File) of
                {ok, Entrys} ->
                    State3 = State2#state{
                        file = File,
                        mtime = MTimeNew,
                        entrys = Entrys
                    },
                    {noreply, State3};
                _Error ->
                    ?Warn("the crontab file ~s format error:~p", [File, _Error]),
                    {noreply, State2}
            end;
        false ->
            {noreply, State2}
    end;
handle_info({timeout, _Ref, check_cron}, State = #state{entrys = Entrys}) ->
    State2 = State#state{
        file_timer = check_cron_timer()
    },
    check_entrys(Entrys),
    {noreply, State2};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
    
%%-----------------------------------------------------------------------------
%%
%% internal API
%%
%%-----------------------------------------------------------------------------

%% start the check file timer
check_file_timer() ->
    erlang:start_timer(?CHECK_FILE_INTERVAL, self(), check_file).

%% start the cron tasks timer
check_cron_timer() ->
    erlang:start_timer(?CHECK_CRON_INTERVAL, self(), check_cron).


%% check the cron entrys
check_entrys(Entrys) ->
    Now = {Date, _Time} = erlang:localtime(), 
    Week = calendar:day_of_the_week(Date),
    lists:foreach(
        fun(Entry) ->
                case can_run(Entry, Now, Week) of
                    true ->
                        run_task(Entry#cron_entry.mfa);
                    false ->
                        ok
                end
        end,
        Entrys).

can_run(Entry, {{_, CurMon, CurDay}, {CurH, CurM, _}}, Week) ->
    #cron_entry{
        m = M,
        h = H,
        dom = Dom,
        mon = Mon,
        dow = Dow
    } = Entry,
    field_ok(M, CurM) andalso
    field_ok(H, CurH) andalso
    (field_ok(Dom, CurDay) orelse field_ok(Dow, Week)) andalso
    field_ok(Mon, CurMon). 

%% check if the field is ok
field_ok(#cron_field{type = ?CRON_NUM, value = Val}, Cur) ->
    Val =:= Cur;
field_ok(#cron_field{type = ?CRON_RANGE, value = {First, Last, Step}}, Cur) ->
    range_ok(Cur, First, Last, Step);
field_ok(#cron_field{type = ?CRON_LIST, value = List}, Cur) ->
    lists:any(
        fun(FInList) ->
                field_ok(FInList, Cur)
        end,
        List).

%% check if the value in the range
range_ok(Val, First, Last, Step) ->
    range_ok1(Val, First, Last, Step).

range_ok1(Val, Val, _Last, _Step) ->
    true;
range_ok1(_Val, Cur, Last, _Step) when Cur >= Last ->
    false;
range_ok1(Val, Cur, Last, Step) ->
    range_ok1(Val, Cur + Step, Last, Step).


%% run the task
run_task({M, F, A} = Task) ->
    proc_lib:spawn(
        fun() ->
            case catch apply(M, F, A) of
                {'EXIT', R} ->
                    ?Error("cron task ~p error: ~p", [Task, R]),
                    ok;
                _ ->
                    ok
            end
        end
    ).
