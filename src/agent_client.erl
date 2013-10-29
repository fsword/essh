-module(agent_client).

-behaviour(gen_server).

-export([start_link/2]).
-export([conn/2, exec/2]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2]).
-export([code_change/3,terminate/2]).

-record(user_id, {user,host,port,conn}).
-define(SERVER(ChannelId), list_to_atom("channel@" ++ integer_to_list(ChannelId))).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(ChannelId, WhoAmI ) ->
  error_logger:info_msg("start link: ~p, ~p", [ChannelId, WhoAmI]),
  gen_server:start_link({local, ?SERVER(ChannelId)}, ?MODULE, WhoAmI,[]).

conn(ChannelId, Password) ->
  %% TODO try to conn client by args
  %% if it fail, then release client immediately
  %% return result according to module api_handler#L30
  error_logger:info_msg("conn: ~p, ~p~n", [ChannelId,Password]),
  gen_server:call(?SERVER(ChannelId), {conn, Password}).

exec(_ChannelId, _Command) -> 
  %% TODO exec Command over channel that ChId indicated
  ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([User,Host,none]) ->
  init([User,Host,22]);
init([User,Host,Port]) ->
  State = #user_id{user=User,host=Host,port=Port},
  {ok, State}.

handle_call({conn,Password}, _From, State=#user_id{host=Host,port=Port}) ->
  error_logger:info_msg("conn: ~p~n", [Password]),
  Options = options(Password,State#user_id.user),
  case ssh:connect(Host, Port, Options) of
    {ok, Conn} ->
      {reply, ok, State#user_id{conn=Conn}};
    {error, Reason} ->
      io:format("error: ~p~n", [Reason]),
      {reply, Reason, fail}
  end;
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
  io:format("Reason: ~p~n", [Reason]),
  io:format("State: ~p~n", [State]),

  terminated.

options(Password, User) ->
  Options = [
    {user, User}, 
    {silently_accept_hosts, true},
    {user_interaction, false},
    {connect_timeout, 10000}
  ],
  case Password of
    none -> Options;
    _ -> [{password, Password}|Options]
  end.

