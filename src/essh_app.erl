-module(essh_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("records.hrl").
-define(C_ACCEPTORS,  2000).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    run_once(),
    essh_sup:start_link().

stop(_State) ->
    %% release supervisor tree
    ok.

run_once() ->
    crypto:start(),
    ssh:start(),
    mnesia:create_schema([node()|nodes()]),
    mnesia:start(),
    mnesia:create_table(command, [
                                  {attributes, record_info(fields, command)},
                                  {type, ordered_set},
                                  {disc_copies, [node()]}
                                 ]),
    mnesia:create_table(channel, [
                                  {attributes, record_info(fields, channel)},
                                  {type, ordered_set},
                                  {disc_copies, [node()]}
                                 ]).


