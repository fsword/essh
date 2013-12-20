-module(essh).

-export([run_once/0]).
-export([start/0,stop/0]).
-export([create/4,remove/2]).
-export([cmd/5,cmd/6]).
-export([exec/3,exec/4,result/3]).

-include("records.hrl").
-define(TIMEOUT, 60000).

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

start() ->
  application:start(essh).

stop() ->
  application:stop(essh).

%% generate a new channel
%% store channel - token pair
%% the action is fail when connect fail
create(User, Host, Port, Password) ->
    Result = essh_client_sup:add_client([User,Host,Port],Password),
    case Result of
        {ok,ChannelId} ->
            Token = essh_store:add_channel(ChannelId),
            {ok, ChannelId,Token};
        Other ->
            Other
    end.

remove(ChannelId, Token) ->
    Result = essh_store:remove_channel(ChannelId,Token),
    case Result of
        true  -> essh_client_sup:remove_client(ChannelId);
        false -> false
    end.

cmd(Command, User, Host, Port, Password) ->
    {ok, Id, Token} = create(User, Host, Port, Password),
    exec(Command, Id, Token).

cmd(Command, User, Host, Port, Password, async) ->
    {ok, ChId, Token} = create(User, Host, Port, Password),
    {ok, CmdId} = exec(Command, ChId, Token, async),
    {ok, ChId, Token, CmdId}.

exec(Command, ChannelId, Token) ->
    exec(Command, ChannelId, Token, ?TIMEOUT).

exec(Command, ChannelId, Token, async) ->
    do_auth(
      ChannelId, Token, 
      fun() -> essh_client:exec(ChannelId, Command) end
    );
exec(Command, ChannelId, Token, Timeout) ->
    do_auth(
      ChannelId, Token, 
      fun() ->
          Result = essh_client:sync_exec(ChannelId, Command),
          essh_receiver:handle(Result, Timeout)
      end
    ).

result(ChannelId, Token, CmdId) when is_binary(Token)->
    result(ChannelId, binary_to_list(Token), CmdId);
result(ChannelId, Token, CmdId) ->
    do_auth(
      ChannelId, Token, 
      fun() -> essh_store:result(CmdId) end
    ).

do_auth(ChannelId, Token, F) ->
    case essh_store:check_channel(ChannelId,Token) of
        ok -> F();
        Other -> Other
    end.
