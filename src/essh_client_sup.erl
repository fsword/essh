-module(essh_client_sup).

-behaviour(supervisor).

-export([start_link/0,add_client/2,remove_client/1]).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_client(ChannelId,WhoAmI) ->
  supervisor:start_child(?MODULE,[ChannelId,WhoAmI]).

remove_client(ChannelId) ->
  essh_client:stop(ChannelId).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Client = { 
    essh_client, 
    { essh_client, start_link, []},
    transient,
    1000,
    worker, 
    [essh_client]
  },
  {ok, { {simple_one_for_one, 10, 60}, [Client]} }.

