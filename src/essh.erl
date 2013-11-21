-module(essh).

-export([start/0]).

start() ->
  application:start(crypto),
  application:start(ranch),
  application:start(cowlib),
  application:start(cowboy),
  application:start(essh).
