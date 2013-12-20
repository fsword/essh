-module(essh_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

-include("records.hrl").
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Fun, Args), {I, {I, Fun, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    crypto:start(),
    ssh:start(),
    mnesia:start(),
    mnesia:wait_for_tables([command], 5000),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    IdGen     = ?CHILD(essh_id_gen,     worker,     start_link, []),
    ClientSup = ?CHILD(essh_client_sup, supervisor, start_link, []),
    {ok, { {one_for_one, 5, 10}, [IdGen,ClientSup]} }.


