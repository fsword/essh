-module(essh_run).

-export([exec/3]).

exec(Conn,Command,CmdId) ->
  {ok, Chl} = ssh_connection:session_channel(Conn, infinity),
  success = ssh_connection:exec(Conn,Chl,Command,infinity),
  loop(CmdId).

loop(Id) ->
  receive
    {ssh_cm, _Conn, {data, _Chl, _Type_code, Data}} ->
      %% ignore the difference of type code
      %% because stdout/stderr are used in different tool by
      %% the different way.
      agent_store:append_out(Id, Data),
      loop(Id);
    {ssh_cm, _Conn, {exit_status, _Chl, ExitStatus}} ->
      agent_store:exit_status(Id, ExitStatus),
      loop(Id); 
    {ssh_cm, _Conn, {eof,_Chl}} ->
      agent_store:merge_out(Id),
      loop(Id);
    {ssh_cm, _Conn, {exit_signal, _Chl, _ExitSignal, _ErrorMsg, _LanguageString}} ->
      %% TODO call From process
      loop(Id);
    {ssh_cm, _Conn, {closed,Chl}} ->
      io:format("closed: ~p", [Chl])
  end.
