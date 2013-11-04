-module(agent_essh_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Fun, Args), {I, {I, Fun, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  ssh:start(),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  IdGen     = ?CHILD(agent_id_gen,    worker,     start_link, []),
  Redis     = ?CHILD(agent_redis,     worker,     start_link, []),
  Service   = ?CHILD(essh_service,    worker,     start_link, []),
  ClientSup = ?CHILD(essh_client_sup, supervisor, start_link, []),
  {ok, { {one_for_one, 5, 10}, [IdGen,Service,Redis,ClientSup]} }.

