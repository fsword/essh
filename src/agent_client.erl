-module(agent_client).

-behaviour(gen_fsm).

-export([start_link/2]).
-export([connect/2, exec/2, stop/1]).
-export([init/1,handle_event/3,handle_sync_event/4,handle_info/3]).
-export([new/2]).
-export([code_change/4,terminate/3]).

-record(data, {user,host,port,conn}).
-define(SERVER(ChannelId), list_to_atom("channel@" ++ integer_to_list(ChannelId))).

%% ===================================================================
%% API functions
%% ===================================================================

%% WhoAmI: [User,Host,Port]
start_link(ChannelId, WhoAmI ) ->
  error_logger:info_msg("start link: ~p, ~p", [ChannelId, WhoAmI]),
  gen_fsm:start_link({local, ?SERVER(ChannelId)}, ?MODULE, WhoAmI,[]).

connect(ChannelId, Password) ->
  error_logger:info_msg("conn: ~p, ~p~n", [ChannelId,Password]),
  gen_fsm:send_event(?SERVER(ChannelId), {connect, Password}).

stop(ChannelId) ->
  error_logger:info_msg("stop: ~p, ~p~n", [ChannelId]),
  gen_fsm:sync_send_all_state_event(?SERVER(ChannelId), stop).

exec(ChannelId, Command) -> 
  error_logger:info_msg("exec(~p), ~p~n", [ChannelId,Command]),
  gen_server:call(?SERVER(ChannelId),{exec,Command}),
  ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([User,Host,none]) ->
  init([User,Host,22]);
init([User,Host,Port]) ->
  StateData = #data{user=User,host=Host,port=Port},
  {ok, new, StateData}.

new({connect,Password}, StateData=#data{host=Host,port=Port}) ->
  error_logger:info_msg("conn: ~p~n", [Password]),
  Options = options(Password,StateData#data.user),
  case ssh:connect(Host, Port, Options) of
    {ok, Conn} ->
      {next_state, normal, StateData#data{conn=Conn}};
    {error, Reason} ->
      io:format("error: ~p~n", [Reason]),
      {stop, {error, Reason}, StateData}
  end.

handle_sync_event(stop, _From, _StateName, StateData) ->
  {stop,normal,"stopped",StateData};
handle_sync_event(Event, _From, StateName, StateData) ->
  error_logger:info_msg("~p: ~p", [Event,StateName]),
  {next_state, StateName, StateData}.

handle_event(Event, StateName, StateData) ->
  error_logger:info_msg("~p: ~p", [Event,StateName]),
  {next_state, StateName, StateData}.

handle_info(Info, StateName, StateData) ->
  error_logger:info_msg(": ~p", [Info]),
  {next_state, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

terminate(Reason, StateName, StateData) ->
  io:format("Reason: ~p~n", [Reason]),
  io:format("State: ~p~n", [StateName]),
  io:format("State: ~p~n", [StateData]),

  terminated.

options(Password, User) ->
  Options = [
    {user, User}, 
    {silently_accept_hosts, true},
    {user_interaction, false},
    {connect_timeout, 2000}
  ],
  case Password of
    none -> Options;
    _ -> [{password, Password}|Options]
  end.

