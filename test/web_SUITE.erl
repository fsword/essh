%% common_test suite for mymodule

-module(web_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

suite() -> [{timetrap, {seconds, 20}}].

groups() -> [].

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

init_per_suite(Config) -> essh_store:run_once(), inets:start(), Config.
end_per_suite(_Config) -> inets:stop(), ok.

init_per_group(_group, Config) -> Config.
end_per_group(_group, Config) -> Config.

init_per_testcase(_TestCase, Config) -> essh:start(), Config.
end_per_testcase(_TestCase, Config) -> Config.

test_async(_Config) ->
    Body   = web_helper:post("http://localhost:8002/api/channels", 
                       [{host, "localhost"}]
                      ),
    [ChId, Token] = string:tokens(Body,"|"),
    CmdId  = web_helper:put("http://localhost:8002/api/channels/"++ChId, 
                      [{token,Token}, {command, "sleep 0.05 && echo hello"}]
                     ),
    timer:sleep(150),
    "hello\n" = web_helper:get("http://localhost:8002/api/commands/"++CmdId, 
                      [{token, Token}, {channel_id, ChId}]
                     ).
%%TODO
%% test web invoke with callback
%% test cancel command
