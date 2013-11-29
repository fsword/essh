-module(essh_store).

-export([run_once/0]).
-export([add_command/0,merge_out/2,exit_status/2,result/1]).
-export([add_channel/1, check_channel/2, remove_channel/2]).
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

add_command() ->
    Id = essh_id_gen:next(command),
    mnesia:dirty_write(#command{id=Id}),
    Id.

merge_out(CmdId,BinOuts) when is_integer(CmdId) ->
    Out = lists:foldl(
            fun(T, AccIn) -> <<AccIn/binary,T/binary>> end,
            <<>>,
            BinOuts
           ),
    update_command(CmdId, fun(Record) -> Record#command{out=Out} end),
    Out.

exit_status(CmdId, Status) when is_integer(CmdId) ->
    update_command(CmdId, fun(Record) -> Record#command{status=Status} end).

update_command(CmdId, F) ->
    mnesia:transaction(fun() ->
                               case mnesia:wread({command, CmdId}) of
                                   [] -> mnesia:write(F(#command{id=CmdId}));
                                   [Cmd|_] -> mnesia:write(F(Cmd))
                               end
                       end
                      ).

result(CmdId) ->
    case mnesia:dirty_read({command, CmdId}) of
        [] -> 
            not_found;
        [#command{status=Status,out=Out}|_] -> 
            {ok, Status, Out}
    end.

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
