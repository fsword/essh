-module(essh).

-export([start/0,stop/0]).
-export([create/4,remove/2]).
-export([cmd/5,cmd/6]).
-export([exec/3,exec/4,result/3]).

-define(TIMEOUT, 60000).

start() ->
  application:start(essh).

stop() ->
  application:stop(essh).

%% generate a new channel
%% store channel - token pair
%% the action is fail when connect fail
create(User, Host, Port, Password) ->
    create(User, Host, Port, Password, none).

create(User, Host, Port, Password, Callback) ->
    Result = essh_client_sup:add_client([User,Host,Port],Password),
    case Result of
        {ok,ChannelId} ->
            Token = essh_store:add_channel(ChannelId),
            case Callback of
                none -> {ok, ChannelId,Token};
                _    -> Callback(ChannelId, Token)
            end;
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
    CbFun = fun(ChId, Token) -> 
                    exec(Command, ChId, Token) 
            end,
    create(User, Host, Port, Password, CbFun).

cmd(Command, User, Host, Port, Password, async) ->
    CbFun = fun(ChId, Token) ->
                    {ok, CmdId} = exec(Command, ChId, Token, async),
                    {ok, ChId, Token, CmdId}
            end,
    create(User, Host, Port, Password, CbFun).

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

do_auth(ChannelId, Token, ContinuationFun) ->
    case essh_store:check_channel(ChannelId,Token) of
        ok -> ContinuationFun();
        Other -> Other
    end.
