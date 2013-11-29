-module(essh_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(C_ACCEPTORS,  2000).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Dispatch  = cowboy_router:compile(routes()),
  Port      = port(),
  TransOpts = [{port, Port}],
  ProtoOpts = [{env, [{dispatch, Dispatch}]}],
  {ok, _}   = cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),
  essh_sup:start_link().

stop(_State) ->
  %% release supervisor tree
  ok.

routes() ->
  Host = '_',
  Paths = [
    {"/api/[...]"      , essh_web_api_handler, []}
  ],
  [ {Host, Paths} ].

port() ->
  case os:getenv("PORT") of
    false ->
      {ok, Port} = application:get_env(http_port),
      Port;
    _Other ->
      8002
  end.