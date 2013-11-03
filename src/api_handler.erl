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
  {User, _}     = cowboy_req:qs_val(<<"user">>,     Req, none),
  {Host, _}     = cowboy_req:qs_val(<<"host">>,     Req, none),
  {Password, _} = cowboy_req:qs_val(<<"password">>, Req, none),
  {Port, _}     = cowboy_req:qs_val(<<"port">>,     Req, none),
  
  Result = agent_channel:create(
    User,Host,
    Port,
    Password
  ),
  case Result of
    {ok, [ChannelId, Token]} ->
      [200, [], <<(ChannelId++" "++Token)>>];
    no_host ->
      [404, [], <<"cannot find the host">>];
    cannot_conn ->
      [503, [], <<"ssh connection fail">>];
    %% 不接受非binary的Msg返回值
    {bad_request, Msg} when is_binary(Msg) ->
      [400, [], Msg]
  end;
dispatch(<<"PUT">>,  [<<"channels">>,ChId], Req) ->
  {Token, _} = cowboy_req:qs_val(<<"token">>, Req, none),
  case agent_channel:is_exist(ChId, Token) of
    ok -> 
      {Command, _} = cowboy_req:qs_val(<<"command">>, Req, none),
      {ok, CommandId} = agent_client:exec(ChId, Command),
      [200, [], <<CommandId>>];
    not_found ->
      [404, [], <<"not found">>];
    not_allow ->
      [401, [], <<"access denied">>]
  end;
dispatch(_Method, _Path, _Req) ->
  [404, [], <<"not found">>]. 
