-module(essh_run).

-export([exec/3]).

exec(Conn,Command,ChannelId) ->
  {ok, Chl} = ssh_connection:session_channel(Conn, infinity),
  success = ssh_connection:exec(Conn,Chl,Command,infinity),
  loop(integer_to_list(ChannelId)).

loop(Id) ->
  receive
    {ssh_cm, _Conn, {data, _Chl, _Type_code, Data}} ->
      %% ignore the difference of type code
      %% because stdout/stderr are used in different tool by
      %% the different way.
      agent_redis:lpush( "c@"++Id, Data),
      loop(Id);
    {ssh_cm, _Conn, {exit_status, _Chl, ExitStatus}} ->
      agent_redis:set( "r@"++Id, ExitStatus),
      loop(Id); 
    {ssh_cm, _Conn, {eof,_Chl}} ->
      %% TODO eof: store whole stdout/stderr
      loop(Id);
    {ssh_cm, _Conn, {exit_signal, _Chl, _ExitSignal, _ErrorMsg, _LanguageString}} ->
      %% TODO call From process
      loop(Id);
    {ssh_cm, _Conn, {closed,_Chl}} ->
      closed %% TODO call From process
  end.
