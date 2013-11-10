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
  %% HasBody = cowboy_req:has_body(Req),
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
    no_host ->
      [404, [], <<"cannot find the host">>];
    cannot_conn ->
      [503, [], <<"ssh connection fail">>];
    %% 不接受非binary的Msg返回值
    {bad_request, Msg} when is_binary(Msg) ->
      [400, [], Msg]
  end;
dispatch(<<"PUT">>,  [<<"channels">>,ChannelId], Req) ->
  {ok, PostVals, _} = cowboy_req:body_qs(Req),
  Token = post_value("token", PostVals),
  ChId  = binary_to_integer(ChannelId),
  case essh_service:auth(ChId, Token) of
    ok -> 
      Command = post_value("command", PostVals),
      {ok, CommandId} = essh_client:exec(ChId, Command),
      [200, [], integer_to_binary(CommandId)];
    not_found ->
      [404, [], <<"not found"    >>];
    not_allow ->
      [401, [], <<"access denied">>]
  end;
dispatch(Method, Path, Req) ->
  io:format("req not found: ~p ~p ~p", [Method, Path, Req]),
  [404, [], <<"not found">>]. 

post_value(Key, PostVals) ->
  case proplists:get_value(list_to_binary(Key), PostVals) of
    undefined -> undefined;
    V -> binary_to_list(V)
  end.
