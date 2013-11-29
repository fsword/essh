-module(essh_client).

-behaviour(gen_fsm).

-export([start_link/2]).
-export([connect/2, exec/2, sync_exec/2, stop/1,fire_event/2]).
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
  error_logger:info_msg("stop: ~p, ~p~n", [ChannelId]),
  gen_fsm:sync_send_all_state_event(?SERVER(ChannelId), stop).

exec(ChannelId, Command) ->
  Id = essh_store:add_command(),
  gen_fsm:send_all_state_event(?SERVER(ChannelId),{exec,{Id,Command}}),
  {ok, Id}.

sync_exec(ChannelId, Command) ->
  Id = essh_store:add_command(),
  gen_fsm:sync_send_all_state_event(?SERVER(ChannelId),{exec,{Id,Command}}),
  {ok, Id}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([ChannelId,User,Host,undefined]) ->
  init([ChannelId,User,Host,22]);
init([ChannelId,User,Host,Port]) ->
  error_logger:info_msg("start link: ~p, ~p~n", [ChannelId, self()]),
  StateData = #data{user=User,host=Host,port=Port,channel=ChannelId},
  {ok, new, StateData}.

handle_sync_event({connect,Password}, _From, _StateName, StateData=#data{host=Host,port=Port,channel=ChannelId}) ->
  Options = options(Password,StateData#data.user),
  case ssh:connect(Host, Port, Options) of
    {ok, Conn} ->
      {reply, {ok,ChannelId}, normal, StateData#data{conn=Conn}};
    {error, Reason} ->
      io:format("error: ~p~n", [Reason]),
      {stop, Reason, {error, Reason}, StateData}
  end;

handle_sync_event(stop, _From, _StateName, StateData) ->
  %% TODO store all cmds and terminate ssh connection
  {stop,normal,true,StateData};

handle_sync_event({exec, {Id,Cmd}}, {Pid,_Tag}, normal, StateData=#data{current=undefined}) ->
  %% assertion: cmds is empty
  do_exec(Cmd,StateData#data.conn),
  {reply, ok, normal, StateData#data{current={Id,Pid},out=[]}};
%% when cmds is not empty, the current cmd must be not undefined.
handle_sync_event({exec, {Id,Cmd}}, {Pid,_Tag}, StateName, StateData=#data{cmds=Cmds}) ->
  NewCmds = lists:append(Cmds, [{Id,Cmd,Pid}]),
  {reply, ok, StateName, StateData#data{cmds=NewCmds}}.

%% just for exec event, so there is no reply definitely
handle_event(Event, StateName, StateData) ->
  {reply, ok, NewName, NewData} = handle_sync_event(Event, {undefined,undefined}, StateName, StateData),
  {next_state, NewName, NewData}.

handle_info({ssh_cm, Conn, {closed,Chl}}, normal, StateData=#data{cmds=[{Id,Cmd,From}|Others]}) ->
    io:format("next(~p,~p)~n", [Conn, Chl]),
    do_exec(Cmd,Conn),
    fire_event(From, close),
    {next_state, normal, StateData#data{cmds=Others,current={Id,From},out=[]}};
handle_info({ssh_cm, Conn, {closed,Chl}}, StateName, StateData=#data{current={_,From}}) ->
    io:format("closed(~p,~p) ~p ~n", [Conn, Chl, StateName]),
    fire_event(From, close),
    {next_state, StateName, StateData#data{current=undefined}};
handle_info({ssh_cm, Conn, {exit_signal, Chl, ExitSignal, ErrMsg, Lang}}, StateName, StateData) ->
    io:format("signal(~p,~p) ~p ~p ~p ~p~n", [Conn, Chl, StateName, ExitSignal, ErrMsg, Lang]),
    {next_state, StateName, StateData};
handle_info({ssh_cm, _Conn, Info}, StateName, StateData=#data{current={Id,From},out=Out}) ->
    io:format("ssh_cm: ~p~n", [Info]),%%TODO remove
    NewOut = case Info of
                 %% ignore the difference of type code
                 %% because stdout/stderr are used in different tool by
                 %% the different way.
                 {data, _Chl, _Type_code, Data} ->
                     fire_event(From, {data, Data}),
                     [Data|Out];
                 {exit_status, _Chl, ExitStatus} ->
                     fire_event(From, {exit, ExitStatus}),
                     essh_store:exit_status(Id, ExitStatus),
                     Out;
                 {eof,_Chl} ->
                     fire_event(From, eof),
                     essh_store:merge_out(Id,Out),
                     []
             end,
    {next_state, StateName, StateData#data{out=NewOut}}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

terminate(Reason, StateName, StateData) ->
  io:format("terminate: ~p ~p ~p~n", [Reason,StateName,StateData]).

fire_event(undefined, _) -> ok;
fire_event(From,  Event) -> From ! Event.

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

do_exec(Cmd, Conn) ->
  {ok, Chl} = ssh_connection:session_channel(Conn, infinity),
  success = ssh_connection:exec(Conn,Chl,Cmd,infinity).

