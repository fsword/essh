-module(essh_store).

-export([add_command/0,update_out/2,update_exit_status/2]).
-export([result/1,origin_result/1,merge/1,cleanup/1,cleanup/2]).
-export([add_channel/1, check_channel/2, remove_channel/2]).
-include("records.hrl").
-include_lib("stdlib/include/qlc.hrl").

add_command() ->
    Id = essh_id_gen:next(command),
    {MegaS, S, _} = os:timestamp(),
    mnesia:dirty_write(#command{id=Id,created_at=MegaS*1000000+S}),
    Id.

update_out(CmdId,BinOuts) when is_integer(CmdId) ->
    update_command(CmdId, fun(Record) -> Record#command{out=BinOuts} end).

update_exit_status(CmdId, Status) when is_integer(CmdId) ->
    update_command(CmdId, fun(Record) -> Record#command{status=Status} end).

update_command(CmdId, F) ->
    error_logger:info_report([{essh_store,update},CmdId]),
    mnesia:transaction(fun() ->
                               case mnesia:wread({command, CmdId}) of
                                   [] -> mnesia:write(F(#command{id=CmdId}));
                                   [Cmd|_] -> mnesia:write(F(Cmd))
                               end
                       end
                      ).

cleanup(Until) ->
    cleanup({0,0,0}, Until).

cleanup({FromMegaS,FromS,_}, {ToMegaS,ToS,_}) ->
    FromSecond = FromMegaS * 1000000 + FromS,
    ToSecond = ToMegaS * 1000000 + ToS,
    F = fun() -> qlc:e(qlc:q([mnesia:delete({command,Id}) || 
                              #command{
                                 created_at=CreatedAt,
                                 id=Id }<- mnesia:table(command), 
                              CreatedAt > FromSecond,
                              CreatedAt < ToSecond
                             ]))
        end,
    mnesia:activity(transaction,F).

result(CmdId) ->
    error_logger:info_report([{essh_store,get_result},CmdId]),
    case mnesia:dirty_read({command, CmdId}) of
        [] -> 
            not_found;
        [#command{status=Status,out=Out}|_] ->
            io:format("output: ~ts", [Out]),
            {ok, Status, merge(Out)}
    end.

origin_result(CmdId) ->
    error_logger:info_report([{essh_store,get_origin},CmdId]),
    case mnesia:dirty_read({command, CmdId}) of
        [] -> 
            not_found;
        [#command{status=Status,out=Out}|_] ->
            {ok, Status, Out}
    end.

merge(undefined) -> undefined;
merge(BinOuts) ->
    lists:foldl(
      fun(T, AccIn) -> <<AccIn/binary,T/binary>> end,
      <<>>,
      BinOuts
    ).

add_channel(ChannelId) ->
    Token = randchar(12),
    mnesia:dirty_write(#channel{id=ChannelId,token=Token}),
    Token.

check_channel(ChannelId, Token) ->
    case mnesia:dirty_read({channel, ChannelId}) of
        [#channel{token=Token}|_] -> ok;
        []                        -> not_found;
        _                         -> not_allow
    end.

remove_channel(ChannelId, _Token) ->
    mnesia:dirty_delete({channel, ChannelId}).

randchar(N) ->
    random:seed(erlang:now()),
    randchar(N, []).

randchar(0, Acc) ->
    Acc;
randchar(N, Acc) ->
    randchar(N - 1, [random:uniform(26) + 96 | Acc]).
