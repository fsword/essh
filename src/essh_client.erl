-module(essh_client).

-behaviour(gen_fsm).

-export([start_link/2]).
-export([connect/2, exec/2, stop/1]).
-export([init/1,handle_event/3,handle_sync_event/4,handle_info/3]).
-export([new/2]).
-export([code_change/4,terminate/3]).

-record(data, {channel,user,host,port,conn,cmds=[],current=none}).
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
  Id = essh_store:add_command(),
  gen_fsm:send_all_state_event(?SERVER(ChannelId),{exec,{Id,Command}}).

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
      io:format("conneted: ~p", [Conn]),
      {next_state, normal, StateData#data{conn=Conn}};
    {error, Reason} ->
      io:format("error: ~p~n", [Reason]),
      {stop, {error, Reason}, StateData}
  end.

handle_sync_event(stop, _From, _StateName, StateData) ->
  %% TODO store all cmds and terminate ssh connection
  {stop,normal,true,StateData}.

handle_event({exec, CmdInfo}, normal, StateData=#data{cmds=[],current=none}) ->
  do_exec(CmdInfo,StateData#data.conn),
  {next_state, normal, StateData#data{current=CmdInfo}};
%% when cmds is not empty, the current cmd must be not none.
handle_event({exec, CmdInfo}, StateName, StateData=#data{cmds=Cmds}) ->
  NewCmds = lists:append(Cmds, [CmdInfo]),
  {next_state, StateName, StateData#data{cmds=[NewCmds]}}.

handle_info({ssh_cm, _Conn, {closed,Chl}}, normal, StateData=#data{cmds=[NextCmdInfo|Others]}) ->
  io:format("closed(~p)~n", [Chl]),
  do_exec(NextCmdInfo,StateData#data.conn),
  {next_state, normal, StateData#data{cmds=Others,current=none}};
handle_info({ssh_cm, _Conn, {closed,Chl}}, StateName, StateData) ->
  io:format("closed(~p) ~p ~n", [StateName,Chl]),
  {next_state, StateName, StateData#data{current=none}};
handle_info({ssh_cm, _Conn, {exit_signal, Chl, ExitSignal, ErrMsg, Lang}}, StateName, StateData) ->
  io:format("signal(~p) ~p ~p ~p ~p~n", [StateName,Chl, ExitSignal, ErrMsg, Lang]),
  {next_state, StateName, StateData};
handle_info({ssh_cm, _Conn, Info}, StateName, StateData=#data{current={Id,_Cmd}}) ->
  case Info of
    %% ignore the difference of type code
    %% because stdout/stderr are used in different tool by
    %% the different way.
    {data, _Chl, _Type_code, Data} ->
      essh_store:append_out(Id, Data);
    {exit_status, _Chl, ExitStatus} ->
      essh_store:exit_status(Id, ExitStatus);
    {eof,_Chl} ->
      essh_store:merge_out(Id)
  end,
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

do_exec({_Id, Cmd}, Conn) ->
  {ok, Chl} = ssh_connection:session_channel(Conn, infinity),
  success = ssh_connection:exec(Conn,Chl,Cmd,infinity).

