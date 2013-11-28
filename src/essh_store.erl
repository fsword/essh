-module(essh_store).
-behaviour(gen_server).

-export([run_once/0]).
-export([start_link/0,add_command/0,append_out/2,merge_out/1,exit_status/2,result/1]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2]).
-export([code_change/3,terminate/2]).

-export([add_channel/2, check_channel/2, remove_channel/2]).
-include("records.hrl").

run_once() ->
    mnesia:create_schema([node()|nodes()]),
    mnesia:start(),
    mnesia:create_table(command, [
                                  {attributes, record_info(fields, command)},
                                  {type, ordered_set},
                                  {disc_copies, [node()]}
                                 ]),
    mnesia:create_table(channel, [
                                  {attributes, record_info(fields, channel)},
                                  {type, ordered_set},
                                  {disc_copies, [node()]}
                                 ]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_command() ->
    Id = essh_id_gen:next(command),
    mnesia:dirty_write(#command{id=Id}),
    Id.

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
    update_command(#command{id=CmdId,out=Out},
                   fun() -> eredis:q(Conn, ["DEL", RedisKey]) end
                  ),
    Out.

exit_status(CmdId, Status) when is_integer(CmdId) ->
    update_command(#command{id=CmdId, status=Status},none).

update_command(Record=#command{id=CmdId,status=Status,out=Out},F) ->
    Result = mnesia:transaction(
               fun() ->
                       case mnesia:wread({command, CmdId}) of
                           [] -> mnesia:write(Record);
                           [Cmd|_] when is_binary(Out) -> 
                               mnesia:write(Cmd#command{out=Out});
                           [Cmd|_] when is_integer(Status) -> 
                               mnesia:write(Cmd#command{status=Status})
                       end
               end
              ),
    case Result of
        {aborted, Info} -> 
            error_logger:error_msg("merge abort(~p) ~p",[CmdId,Info]);
        {atomic, _} when is_function(F) -> F();
        {atomic, _} -> atomic
    end.

result(CmdId) ->
    case mnesia:dirty_read({command, CmdId}) of
        [] -> 
            not_found;
        [#command{status=Status,out=Out}|_] -> 
            {ok, Status, Out}
    end.

add_channel(ChannelId, Token) ->
    mnesia:dirty_write(#channel{id=ChannelId,token=Token}).

check_channel(ChannelId, Token) ->
    case mnesia:dirty_read({channel, ChannelId}) of
        [#channel{token=Token}|_] -> ok;
        []                        -> not_found;
        _                         -> not_allow
    end.

remove_channel(ChannelId, _Token) ->
    mnesia:dirty_delete({channel, ChannelId}).

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
