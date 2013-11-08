-module(essh_service).

-behaviour(gen_server).

-export([start_link/0,create/4,remove/2,auth/2,exec/3]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2]).
-export([code_change/3,terminate/2]).

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
      gen_server:call(?MODULE, {add,ChannelId,Token});
    Other ->
      essh_client:stop(ChannelId),
      Other
  end.

remove(ChannelId, Token) ->
  Result = gen_server:call(?MODULE, {remove,ChannelId,Token}),
  case Result of
    true ->
      essh_client_sup:remove_client(ChannelId);
    false ->
      false
  end.

exec(Command, ChannelId, Token) ->
  case gen_server:call(?MODULE, {auth,ChannelId,Token}) of
    ok ->
      essh_client:exec(ChannelId, Command);
    Other ->
      Other
  end.

auth(ChannelId, Token) ->
  gen_server:call(?MODULE, {auth,ChannelId,Token}).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

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
  {reply, {ok, ChannelId, Token}, NewDict}.

handle_info(Info, State) ->
  error_logger:info_msg(": ~p", [Info]),
  {noreply, State}.

handle_cast(Request, State) ->
  error_logger:info_msg(": ~p", [Request]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(Reason, State) ->
  io:format("Reason: ~p~n", Reason),
  io:format("State: ~p~n", State),

  terminated.

randchar(N) ->
  randchar(N, []).

randchar(0, Acc) ->
  Acc;
randchar(N, Acc) ->
  randchar(N - 1, [random:uniform(26) + 96 | Acc]).
