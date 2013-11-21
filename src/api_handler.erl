-module(api_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {PathInfo,Req}   = cowboy_req:path_info(Req),
  {Method,Req} = cowboy_req:method(Req),
  io:format("request [~p] ~p~n", [Method,PathInfo]),

  [Status, Headers, Content] = dispatch(Method, PathInfo, Req),

  {ok, Req2} = cowboy_req:reply(Status, Headers, Content, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.

dispatch(<<"POST">>, [<<"channels">>], Req) ->
    {ok, PostVals, _} = cowboy_req:body_qs(Req),

    User     = post_value("user",     PostVals),
    Host     = post_value("host",     PostVals),
    Port     = post_value("port",     PostVals),
    Password = post_value("password", PostVals),

    case essh_service:create(User,Host,Port,Password) of
        {ok, ChannelId, Token} ->
            D = integer_to_list(ChannelId)++"|"++Token,
            io:format("channel with token: ~p~n", [D]),
            [200, [], list_to_binary(D)];
        Other -> default_action(Other)
    end;
dispatch(<<"PUT">>,  [<<"channels">>,ChnIdStr], Req) ->
    {ok, PostVals, _} = cowboy_req:body_qs(Req),
    Token = post_value("token", PostVals),
    ChnId  = binary_to_integer(ChnIdStr),
    Command = post_value("command", PostVals),

    case essh_service:async_exec(Command, ChnId, Token) of
        {ok, CommandId} ->
            [200, [], integer_to_binary(CommandId)];
        Other -> default_action(Other)
    end;
dispatch(<<"GET">>,  [<<"commands">>,CmdIdStr], Req) ->

    CmdId = binary_to_integer(CmdIdStr),
    {ChnIdStr, _} = cowboy_req:qs_val(<<"channel_id">>, Req),
    ChnId = binary_to_integer(ChnIdStr),
    {Token   , _} = cowboy_req:qs_val(<<"token">>     , Req),

    io:format("get ~p ~p ~p~n", [Token, ChnId, CmdId]),
    case essh_service:result(ChnId, Token, CmdId) of
        {ok, _, Out} -> [200, [], Out];
        Other        -> default_action(Other)
    end;
dispatch(Method, Path, Req) ->
    io:format("req not found: ~p ~p ~p", [Method, Path, Req]),
    [404, [], <<"not found">>]. 

post_value(Key, PostVals) ->
  case proplists:get_value(list_to_binary(Key), PostVals) of
    undefined -> undefined;
    V -> binary_to_list(V)
  end.

default_action(no_host) ->
    [404, [], <<"cannot find the host">>];
default_action(not_found) ->
    [404, [], <<"not found"    >>];
default_action(not_allow) ->
    [401, [], <<"access denied">>];
default_action(cannot_conn) ->
    [503, [], <<"ssh connection fail">>];
%% 不接受非binary的Msg返回值
default_action({bad_request, Msg}) when is_binary(Msg) ->
    [400, [], Msg].

