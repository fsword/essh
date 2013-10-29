-module(agent_channel).

-behaviour(gen_server).

-export([start_link/0,create/4]).

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
  %% generate a channel id
  %% add a client with the channel id
  %% try to connect the target server
  %% if success
  %%   create channel and save it.
  %% else
  %%   return error information.
  %%
  ChannelId = agent_id_gen:next(channel),
  agent_client_sup:add_client(ChannelId,[User,Host,Port]),
  Result = agent_client:conn(ChannelId, Password),
  case Result of
    ok -> 
      ChannelId;
    Other ->
      agent_client_sup:remove_client(ChannelId),
      Other
  end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(X) ->
  error_logger:info_msg("channel init: ~p~n", [X]),
  {ok, inited}.

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
