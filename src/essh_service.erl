-module(essh_service).

-export([create/4,remove/2,auth/2]).
-export([async_exec/3,sync_exec/3,sync_exec/4,result/3]).

-define(TIMEOUT, 60000).

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

async_exec(Command, ChannelId, Token) ->
    do_auth(
      ChannelId, Token, 
      fun() -> essh_client:exec(ChannelId, Command) end
    ).

sync_exec(Command, ChannelId, Token) ->
    sync_exec(Command, ChannelId, Token, ?TIMEOUT).

sync_exec(Command, ChannelId, Token, Timeout) ->
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

auth(ChannelId, Token) ->
    essh_store:check_channel(ChannelId,Token).


