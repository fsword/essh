-module(essh_client).

-behaviour(gen_fsm).

-export([start_link/2]).
-export([connect/2, exec/2, stop/1]).
-export([init/1,handle_event/3,handle_sync_event/4,handle_info/3]).
-export([new/2]).
-export([code_change/4,terminate/3]).

-record(data, {channel,user,host,port,conn,cmds=[],handle=none}).
-define(SERVER(ChannelId), list_to_atom("channel@" ++ integer_to_list(ChannelId))).

%% ===================================================================
%% API functions
%% ===================================================================

%% WhoAmI: [User,Host,Port]
start_link(ChannelId, WhoAmI ) ->
  error_logger:info_msg("start link: ~p, ~p", [ChannelId, WhoAmI]),
  gen_fsm:start_link({local, ?SERVER(ChannelId)}, ?MODULE, [ChannelId|WhoAmI],[]).

connect(ChannelId, Password) ->
  gen_fsm:send_event(?SERVER(ChannelId), {connect, Password}).

stop(ChannelId) ->
  error_logger:info_msg("stop: ~p, ~p~n", [ChannelId]),
  gen_fsm:sync_send_all_state_event(?SERVER(ChannelId), stop).

exec(ChannelId, Command) -> 
  gen_fsm:send_all_state_event(?SERVER(ChannelId),{exec,Command}).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([ChannelId,User,Host,none]) ->
  init([ChannelId,User,Host,22]);
init([ChannelId,User,Host,Port]) ->
  StateData = #data{user=User,host=Host,port=Port,channel=ChannelId},
  {ok, new, StateData}.

new({connect,Password}, StateData=#data{host=Host,port=Port}) ->
  Options = options(Password,StateData#data.user),
  case ssh:connect(Host, Port, Options) of
    {ok, Conn} ->
      io:format("connetion: ~p", [Conn]),
      {next_state, normal, StateData#data{conn=Conn}};
    {error, Reason} ->
      io:format("error: ~p~n", [Reason]),
      {stop, {error, Reason}, StateData}
  end.

handle_sync_event(stop, _From, _StateName, StateData) ->
  %% TODO store all cmds and terminate ssh connection
  {stop,normal,true,StateData}.

handle_event({exec, Command}, normal, StateData=#data{cmds=[],handle=none,channel=ChannelId}) ->
  Handle = spawn(fun() -> essh_run:exec(StateData#data.conn,Command,ChannelId) end),
  {next_state, normal, StateData#data{handle=Handle}};
%% when cmds is not empty, the current cmd must be not none. 
handle_event({exec, Command}, StateName, StateData=#data{cmds=Cmds,handle=_Handle}) ->
  NewCmds = lists:append(Cmds, [Command]),
  {next_state, StateName, StateData#data{cmds=[NewCmds]}}.

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

