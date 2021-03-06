%% common_test suite for mymodule

-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%--------------------------------------------------------------------
suite() -> [{timetrap, {seconds, 20}}].

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% Description: Returns a list of test case group definitions.
%%--------------------------------------------------------------------
groups() -> [].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%%
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%
%%      NB: By default, we export all 1-arity user defined functions
%%--------------------------------------------------------------------
all() ->
    [ {exports, Functions} | _ ] = ?MODULE:module_info(),
    [ FName || {FName, _} <- lists:filter(
                               fun ({module_info,_}) -> false;
                                   ({all,_}) -> false;
                                   ({init_per_suite,1}) -> false;
                                   ({end_per_suite,1}) -> false;
                                   ({_,1}) -> true;
                                   ({_,_}) -> false
                               end, Functions)].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Description: Initialization before the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% Description: Initialization before each test case group.
%%--------------------------------------------------------------------
init_per_group(_group, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% Description: Cleanup after each test case group.
%%--------------------------------------------------------------------
end_per_group(_group, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Description: Initialization before each test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    essh:start(),
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for failing the test case.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
    essh:stop(),
    Config.

test_essh() ->
    [{userdata,[{doc,"Testing the essh supervisor of agent module"}]}].

test_exec_async(_Config) ->
    {ok, Id, Token} = essh:create(user(),host(),port(),passwd()),
    error_logger:info_msg("async conn created"),
    {ok, CmdId1} = essh:exec("echo sth && sleep 0.1 && echo cmd1_after_0_1", Id, Token, async),
    {ok, CmdId2} = essh:exec("nohup date 2>&1 >/dev/null && echo cmd2_after_0_05 && sleep 0.05 &", Id, Token, async),
    timer:sleep(270), %% session create(80ms, 40ms) + sleep time(0.1s, 0.05s)
    {ok, _, _} = essh:result(Id, Token, CmdId1),
    {ok, _, _} = essh:result(Id, Token, CmdId2).

test_exec_sync(_Config) ->
    {ok, Id, Token} = essh:create(user(),host(),port(),passwd()),
    {ok, 0, _} = essh:exec("date && sleep 0.1 && echo finish", Id, Token),
    {ok, 0, _} = essh:exec("nohup date 2>&1 >/dev/null && date && sleep 0.05", Id, Token).

test_exec_large_output(_Config) ->
    {ok, Id, Token} = essh:create(user(),host(),port(),passwd()),
    {ok, 0, _} = essh:exec("for i in {1..1000}; do echo -n 'sampledata'; done", Id, Token, 20000).

test_cmd_sync(_Config) ->
    Cmd="echo sync_ok && sleep 0.1 && echo finish",
    {ok, 0,<<"sync_ok\nfinish\n">>} = essh:cmd(Cmd,user(),host(),port(),passwd()).

test_cleanup(_Config) ->
    timer:sleep(1000),
    N = essh_id_gen:fetch(command),
    N > 0,
    essh_store:cleanup(os:timestamp()),
    lists:map(fun(Id) -> 
                      io:format("~p~n ",[Id]),
                      not_found = essh_store:origin_result(Id) 
              end, lists:seq(1,N)).

test_cmd_callback(_Config) ->
    Cmd="echo sync_ok && sleep 0.1 && echo finish",
    Callback = fun(_Id,Event) -> error_logger:info_msg("callback: ~p~n",[Event]) end,
    {ok, _, _, _} = essh:cmd(Cmd,user(),host(),port(),passwd(),Callback),
    timer:sleep(1000).

test_cmd_async(_Config) ->
    Cmd="nohup date 2>&1 >/dev/null && echo async_ok && sleep 0.05",
    {ok, ChId, Token, CmdId} = essh:cmd(Cmd, user(),host(),port(),passwd(),async),
    timer:sleep(500),
    {_, 0, <<"async_ok\n">>} = essh:result(ChId, Token, CmdId).

test_cmd_fail(_Config) ->
    Cmd="echo sync_ok && sleep 0.1 && echo finish",
    {error,econnrefused} = essh:cmd(Cmd,user(),host(),1029,passwd()),
    {error,econnrefused} = essh:cmd(Cmd,user(),host(),1029,passwd(),async).

host()     -> "localhost".
port()     -> 22.
user()     -> undefined.
passwd() -> undefined.

