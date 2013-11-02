-module(agent_redis).

-behaviour(gen_server).

-export([start_link/0, lpush/2]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2]).
-export([code_change/3,terminate/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

lpush(K,V) ->
  Conn = gen_server:call(?MODULE, conn),
  eredis:q(Conn, ["LPUSH", K,V]).

init([]) ->
  eredis:start_link().

handle_call(conn, _From, Conn) ->
  {reply, Conn, Conn}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.
