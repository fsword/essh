-module(agent_store).
-behaviour(gen_server).

-export([start_link/0, push/2, range/2, set/2, fetch/1]).
-export([append_out/2,merge_out/1,exit_status/2]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2]).
-export([code_change/3,terminate/2]).
-include("records.hrl").

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

append_out(CmdId, Out) when is_integer(CmdId) ->
  append_out(integer_to_list(CmdId), Out);
append_out(CmdId, Out) ->
  Conn = gen_server:call(?MODULE, conn),
  eredis:q(Conn, ["RPUSH", "o@"++CmdId,Out]).

merge_out(CmdId) when is_integer(CmdId) ->
  Conn = gen_server:call(?MODULE, conn),
  RedisKey = "o@"++integer_to_list(CmdId),
  {ok, BinOuts} = eredis:q(Conn, ["LRANGE", RedisKey, 0, 20000]),
  Out = lists:foldl(
    fun(T, AccIn) -> <<AccIn/binary,T/binary>> end,
    <<>>,
    BinOuts
  ),
  {Result, Info} = mnesia:transaction(fun() ->
        case mnesia:wread({command, CmdId}) of
          [] -> mnesia:write(#command{id=CmdId,out=Out});
          [Cmd|_] -> 
            mnesia:write(Cmd#command{out=Out})
        end
    end
  ),
  case Result of
    aborted -> error_logger:error_msg("merge abort(~p) ~p",[CmdId,Info]);
    atomic  ->
      eredis:q(Conn, ["DEL", RedisKey]),
      Out
  end.

exit_status(CmdId, Status) when is_integer(CmdId) ->
  mnesia:transaction(fun() ->
        case mnesia:wread({command, CmdId}) of
          [] -> mnesia:write(#command{id=CmdId,status=Status});
          [Cmd|_] -> 
            mnesia:write(Cmd#command{status=Status})
        end
    end
  ).

push(K,V) ->
  Conn = gen_server:call(?MODULE, conn),
  eredis:q(Conn, ["RPUSH", K,V]).

range(K, N) ->
  Conn = gen_server:call(?MODULE, conn),
  eredis:q(Conn, ["LRANGE", K, 0, N]).

set(K,V) ->
  Conn = gen_server:call(?MODULE, conn),
  eredis:q(Conn, ["SET", K,V]).

fetch(K) ->
  Conn = gen_server:call(?MODULE, conn),
  eredis:q(Conn, ["GET", K]).

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
