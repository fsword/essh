-module(agent_id_gen).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([next/1]).

%% Gen Server callbacks
-export([init/1,handle_call/3,handle_cast/2,handle_info/2]).
-export([code_change/3,terminate/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

next(Key) when is_atom(Key) ->
  gen_server:call(?MODULE, {next, Key}).

get(Key) when is_atom(Key) ->
  gen_server:call(?MODULE, {get, Key}).

init([]) ->
  {ok, none}.

handle_call({get,Key}, _From, State) ->
  get(Key);
handle_call({next,Key}, _From, State) ->
  case get(Key) of
    undefined -> 
      put(Key, 1),
      {reply,1,State};
    Num when is_number(Num) ->
      put(Key, Num+1),
      {reply,Num + 1,State}
  end.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.
