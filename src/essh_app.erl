-module(essh_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(C_ACCEPTORS,  2000).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  essh_sup:start_link().

stop(_State) ->
  %% release supervisor tree
  ok.
