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

  [Status, Headers, Content] = dispatch(Method, PathInfo),

  {ok, Req2} = cowboy_req:reply(Status, Headers, Content, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.

dispatch(<<"POST">>, [<<"channels">>]) ->
  %% TODO 创建channel
  [200, [], "ok"];
dispatch(<<"PUT">>,  [<<"channels">>,_ChId]) ->
  %% TODO 发送ssh命令
  [200, [], <<"ok">>];
dispatch(_Method, _Path) ->
  [404, [], <<"not found">>]. 
