-module(essh_client).

-behaviour(gen_fsm).

-export([start_link/2,normal/2]).
-export([connect/2, exec/3, sync_exec/2, stop/1,fire_event/2]).
-export([init/1,handle_event/3,handle_sync_event/4,handle_info/3]).
-export([code_change/4,terminate/3]).

-record(data, {channel,user,host,port,conn,cmds=[],current,out}).
-define(SERVER(ChannelId), list_to_atom("channel@" ++ integer_to_list(ChannelId))).

%% ===================================================================
%% API functions
%% ===================================================================

%% WhoAmI: [User,Host,Port]
start_link(ChannelId, WhoAmI ) ->
  gen_fsm:start_link({local, ?SERVER(ChannelId)}, ?MODULE, [ChannelId|WhoAmI],[]).

connect(ChannelId, Password) ->
  gen_fsm:sync_send_all_state_event(?SERVER(ChannelId), {connect, Password}).

stop(ChannelId) ->
  error_logger:info_report([{stop,ChannelId}]),
  gen_fsm:sync_send_all_state_event(?SERVER(ChannelId), stop).

exec(ChannelId, Command, CbFunc) ->
  Id = essh_store:add_command(),
  gen_fsm:send_all_state_event(?SERVER(ChannelId),{exec,{Id,Command,CbFunc}}),
  {ok, Id}.

sync_exec(ChannelId, Command) ->
  Id = essh_store:add_command(),
  gen_fsm:sync_send_all_state_event(?SERVER(ChannelId),{exec,{Id,Command,undefined}}),
  {ok, Id}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([ChannelId,User,Host,undefined]) ->
  init([ChannelId,User,Host,22]);
init([ChannelId,User,Host,Port]) ->
  error_logger:info_report([{start,ChannelId}, self()]),
  StateData = #data{user=User,host=Host,port=Port,channel=ChannelId},
  {ok, new, StateData}.

normal(timeout, StateData=#data{conn=Conn}) when is_pid(Conn) ->
    ssh:close(Conn),
    {stop, normal, StateData};
normal(timeout, StateData) ->
    {stop, normal, StateData}.

handle_sync_event({connect,Password}, _From, _StateName, StateData=#data{host=Host,port=Port,channel=ChannelId}) ->
  Options = options(Password,StateData#data.user),
  case ssh:connect(Host, Port, Options) of
    {ok, Conn} ->
      error_logger:info_report([{connect,ok},Conn]),
      {reply, {ok,ChannelId}, normal, StateData#data{conn=Conn}};
    {error, Reason} ->
      error_logger:info_report([{connect,error},Reason]),
      {stop, Reason, {error, Reason}, StateData}
  end;

handle_sync_event(stop, _From, _StateName, StateData) ->
  %% TODO store all cmds and terminate ssh connection
  {stop,normal,stopped,StateData};

handle_sync_event({exec, {Id,Cmd,CbFunc}}, {Pid,_Tag}, normal, StateData=#data{current=undefined}) ->
  []=StateData#data.cmds, %% assert cmds is empty for fastfail
  F = do_exec(Id,Cmd,StateData#data.conn,CbFunc),
  {reply, ok, normal, StateData#data{current={Id,Pid,F},out=[]}};
%% when cmds is not empty, the current cmd must be not undefined.
handle_sync_event({exec, {Id,Cmd,CbFunc}}, {Pid,_Tag}, StateName, StateData=#data{cmds=Cmds}) ->
  NewCmds = lists:append(Cmds, [{Id,Cmd,Pid,CbFunc}]),
  {reply, ok, StateName, StateData#data{cmds=NewCmds}}.

%% just for exec event, so there is no reply definitely
handle_event(Event, StateName, StateData) ->
  {reply, ok, NewName, NewData} = handle_sync_event(Event, {undefined,undefined}, StateName, StateData),
  {next_state, NewName, NewData}.

handle_info({ssh_cm, Conn, {closed,Chl}}, normal, StateData=#data{current={_,From,CbFunc},cmds=[NewData|Others]}) ->
    error_logger:info_report([{ssh_cm, closed},{conn,Conn},{chl,Chl},next]),
    fire_event(From, CbFunc, close),
    {NewId, NewCmd, NewFrom, NewCbFunc} = NewData,
    F = do_exec(NewId,NewCmd,Conn,NewCbFunc),
    {next_state, normal, StateData#data{cmds=Others,current={NewId,NewFrom,F},out=[]}};
handle_info({ssh_cm, Conn, {closed,Chl}}, StateName, StateData=#data{current={_,From,CbFunc}}) ->
    error_logger:info_report([{ssh_cm, closed},{conn,Conn},{chl,Chl},StateName]),
    fire_event(From, CbFunc, close),
    {next_state, StateName, StateData#data{current=undefined}, 10000};
handle_info({ssh_cm, Conn, {exit_signal, Chl, ExitSignal, ErrMsg, Lang}}, StateName, StateData) ->
    error_logger:info_report([{ssh_cm,signal},{conn,Conn},{chl,Chl}, StateName, ExitSignal, ErrMsg, Lang]),
    {next_state, StateName, StateData};
handle_info({ssh_cm, Conn, Info}, StateName, StateData=#data{current={Id,From,CbFunc},out=Out}) ->
    NewOut = case Info of
                 %% ignore the difference of type code
                 %% because stdout/stderr are used in different tool by
                 %% the different way.
                 {data, Chl, _Type_code, Data} ->
                     ssh_connection:adjust_window(Conn, Chl, 100000),
                     fire_event(From, CbFunc, {data, Data}),
                     [Data|Out];
                 {exit_status, Chl, ExitStatus} ->
                     error_logger:info_report([{ssh_cm,exit_status},{conn,Conn},{chl,Chl},Info]),
                     fire_event(From, CbFunc, {exit, ExitStatus}),
                     essh_store:update_exit_status(Id, ExitStatus),
                     Out;
                 {eof,Chl} ->
                     error_logger:info_report([{ssh_cm,eof},{conn,Conn},{chl,Chl},Info]),
                     fire_event(From, CbFunc, eof),
                     essh_store:update_out(Id,Out),
                     []
             end,
    {next_state, StateName, StateData#data{out=NewOut}}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

terminate(Reason, StateName, StateData) ->
  error_logger:info_report([{terminate,self()},Reason,StateName,StateData]).

fire_event(From, CbFunc, Event) -> 
    fire_event(From, Event),
    fire_event(CbFunc, Event).

fire_event(undefined, _)                           -> ok;
fire_event(CbFunc, Event) when is_function(CbFunc) -> CbFunc(Event);
fire_event(From, Event)                            -> From ! Event.

options(Password, User) ->
  CommonOptions = [
    {silently_accept_hosts, true},
    {user_interaction, false},
    {connect_timeout, 2000}
  ],
  password(Password, user(User, CommonOptions)).

password(undefined, Options) -> Options;
password(Password,  Options) -> [{password, Password}|Options].

user(undefined, Options)     -> Options;
user(User,      Options)     -> [{user, User}|Options].

do_exec(Id, Cmd, Conn, CbFunc) ->
  {ok, Chl} = ssh_connection:session_channel(Conn, infinity),
  error_logger:info_report([{ssh_cm,create_session},{conn,Conn},{chl,Chl}]),
  success = ssh_connection:exec(Conn,Chl,Cmd,infinity),
  case CbFunc of
      undefined -> undefined;
      _ when is_function(CbFunc) ->
          CbFunc(Id, add),
          fun(Event) -> CbFunc(Id, Event) end
  end.

