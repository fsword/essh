-module(essh_client_sup).

-behaviour(supervisor).

-export([start_link/0,add_client/2]).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_client(WhoAmI,Password) ->
    ChannelId = essh_id_gen:next(channel),
    supervisor:start_child(?MODULE,[ChannelId,WhoAmI]),
    essh_client:connect(ChannelId, Password).

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

