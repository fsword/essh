-module(agent_channel).

-behaviour(gen_server).

-export([start_link/0,create/4,remove/2,auth/2]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2]).
-export([code_change/3,terminate/2]).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [],[]).

%% generate a new channel
%% add client for the channel
create(User, Host, Port, Password) ->
  ChannelId = agent_id_gen:next(channel),
  agent_client_sup:add_client(ChannelId,[User,Host,Port]),
  Result = agent_client:connect(ChannelId, Password),
  case Result of
    ok ->
      Token = randchar(12),
      gen_server:call(?MODULE, {add,ChannelId,Token});
    Other ->
      agent_client_sup:remove_client(ChannelId),
      Other
  end.

remove(ChannelId, Token) ->
  Result = gen_server:call(?MODULE, {remove,[ChannelId, Token]}),
  case Result of
    true ->
      agent_client_sup:remove_client(ChannelId);
    false ->
      false
  end.

auth(ChannelId, Token) ->
  gen_server:call(?MODULE, {auth,[ChannelId, Token]}).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(X) ->
  error_logger:info_msg("channel init: ~p~n", [X]),
  {ok, dict:new()}.

handle_call({auth,{ChannelId, Token}}, _From, Dict) ->
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
  {reply, {ok, [ChannelId, Token]}, NewDict};
handle_call(Request, _From, State) ->
  error_logger:info_msg(": ~p", [Request]),
  {reply, none, State}.

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
