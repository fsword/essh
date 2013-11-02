-module(agent_run).

-export([exec/3]).

exec(Conn,Command,_From) ->
  {ok, Chl} = ssh_connection:session_channel(Conn, infinity),
  success = ssh_connection:exec(Conn,Chl,Command,infinity),
  loop().

loop() ->
  receive
    {ssh_cm, _Conn, {data, _Chl, _Type_code, Data}} ->
      agent_redis:lpush( "ssh", Data),
      loop();
    {ssh_cm, _Conn, {exit_status, _Chl, ExitStatus}} ->
      agent_redis:lpush( "ssh", "exit"),
      agent_redis:lpush( "ssh", ExitStatus),
      loop(); 
    {ssh_cm, _Conn, {eof,_Chl}} ->
      agent_redis:lpush( "ssh", eof),
      loop();
    {ssh_cm, _Conn, {exit_signal, _Chl, _ExitSignal, _ErrorMsg, _LanguageString}} ->
      agent_redis:lpush( "ssh", "sig"),
      loop();
    {ssh_cm, _Conn, {closed,_Chl}} ->
      agent_redis:lpush( "ssh", "close")
  end.
