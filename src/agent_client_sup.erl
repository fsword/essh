-module(agent_client_sup).

-behaviour(supervisor).

-export([start_link/0,add_client/2]).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_client(ChannelId,WhoAmI) ->
  supervisor:start_child(?MODULE,[ChannelId,WhoAmI]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Client = { 
    agent_client, 
    { agent_client, start_link, []},
    transient,
    2000,
    worker, 
    [agent_client]
  },
  {ok, { {simple_one_for_one, 10, 60}, [Client]} }.

