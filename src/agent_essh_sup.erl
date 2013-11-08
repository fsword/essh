-module(agent_essh_sup).
-behaviour(supervisor).

%% API
-export([start_link/0,run_once/0]).
%% Supervisor callbacks
-export([init/1]).

-include("records.hrl").
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Fun, Args), {I, {I, Fun, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  ssh:start(),
  mnesia:start(),
  mnesia:wait_for_tables([command], 5000),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  IdGen     = ?CHILD(essh_id_gen,     worker,     start_link, []),
  Store     = ?CHILD(essh_store,      worker,     start_link, []),
  Service   = ?CHILD(essh_service,    worker,     start_link, []),
  ClientSup = ?CHILD(essh_client_sup, supervisor, start_link, []),
  {ok, { {one_for_one, 5, 10}, [IdGen,Service,Store,ClientSup]} }.

run_once() ->
  mnesia:create_schema([node()|nodes()]),
  mnesia:start(),
  mnesia:create_table(command, [
      {attributes, record_info(fields, command)}
    ]).
