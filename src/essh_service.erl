-module(essh_service).

-behaviour(gen_server).

-export([start_link/0]).
-export([create/4,remove/2,auth/2]).
-export([async_exec/3,sync_exec/3,sync_exec/4,result/3]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2]).
-export([code_change/3,terminate/2]).

-define(TIMEOUT, 60000).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],[]).

%% generate a new channel
%% store channel - token pair
%% the action is fail when connect fail
create(User, Host, Port, Password) ->
    ChannelId = essh_id_gen:next(channel),
    essh_client_sup:add_client(ChannelId,[User,Host,Port]),
    Result = essh_client:connect(ChannelId, Password),
    case Result of
        ok ->
            Token = randchar(12),
            gen_server:call(?MODULE, {add,ChannelId,Token}),
            {ok, ChannelId,Token};
        Other ->
            Other
    end.

remove(ChannelId, Token) ->
    Result = gen_server:call(?MODULE, {remove,ChannelId,Token}),
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

result(ChannelId, Token, CmdId) ->
    do_auth(
      ChannelId, Token, 
      fun() -> essh_store:result(CmdId) end
    ).

do_auth(ChannelId, Token, F) ->
    case gen_server:call(?MODULE, {auth,ChannelId,Token}) of
        ok -> F();
        Other -> Other
    end.

auth(ChannelId, Token) ->
    gen_server:call(?MODULE, {auth,ChannelId,Token}).

%% =========================================================
%% Supervisor callbacks
%% =========================================================

init([]) ->
    {ok, dict:new()}.

handle_call({auth,ChannelId,Token}, _From, Dict) ->
    case dict:find(ChannelId, Dict) of
        error ->
            {reply, not_found, Dict};
        {ok, Token} ->
            {reply, ok, Dict};
        {ok, _} ->
            {reply, not_allow, Dict}
    end;
handle_call({remove,ChannelId,Token}, _From, Dict) ->
    case dict:find(ChannelId, Dict) of
        {ok, Token} ->
            NewDict = dict:erase(ChannelId, Dict),
            {reply, true, NewDict};
        _ -> 
            {reply, false, Dict}
    end;
handle_call({add,ChannelId,Token}, _From, Dict) ->
    NewDict = dict:store(ChannelId, Token, Dict),
    {reply, ok, NewDict}.

handle_info(Info, State) ->
    error_logger:info_msg(": ~p", [Info]),
    {noreply, State}.

handle_cast(Request, State) ->
    error_logger:info_msg(": ~p", [Request]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, State) ->
    io:format("Reason: ~p ~p~n", [Reason, State]).

randchar(N) ->
    randchar(N, []).

randchar(0, Acc) ->
    Acc;
randchar(N, Acc) ->
    randchar(N - 1, [random:uniform(26) + 96 | Acc]).
