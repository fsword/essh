-module(web_helper).
-export([get/2,post/2,put/2]).
-export([join_params/1, string_to_utf8/1, list_to_hex/1]).

-define(DEFAULT_HTTP_OPTIONS, [{connect_timeout, 1000}]).
-define(DEFAULT_OPTIONS, [{sync,true}]).

get(Url, Params) ->
    {ok, {_,_,Body}} = httpc:request(Url ++ "?" ++ join_params(Params)),
    Body.

post(Url, Params) ->
    request(post, Url, Params).

put(Url, Params) ->
    request(put, Url, Params).

request(Method, Url, Params) ->
    ContentType = "application/x-www-form-urlencoded",
    {ok, {_,_,Body}} = httpc:request(Method,
                                     { Url, 
                                       [], ContentType, 
                                       join_params(Params) 
                                     },
                                     ?DEFAULT_HTTP_OPTIONS,?DEFAULT_OPTIONS
                                    ),
    Body.

%% 将列表[{a,"b"},{c,"d"},{x,"y"}]拼接为a=b&c=d&x=y格式，
%% 其中value将会被转码为UTF8格式，如“中文”会被转换为"%E4%B8%AD"，用于url请求
join_params(Ps) ->
  join_params(Ps, "").

join_params([],Acc) ->
    Acc;
join_params([{Key,Value} | R], "") ->
  join_params(R, atom_to_list(Key) ++ "=" ++ any_to_string(Value));
join_params([{Key,Value} | R], Acc) ->
  join_params(R, Acc ++ "&" ++ atom_to_list(Key) ++ "=" ++ any_to_string(Value)).

any_to_string(Value) ->
  if
    is_integer(Value) -> integer_to_list(Value);
    is_atom(Value) ->
      atom_to_list(Value);
    true ->
      string_to_utf8(Value)
  end.

string_to_utf8(L) ->
  lists:foldl(fun(X, Result) -> Result ++ "%" ++ X  end, "", list_to_hex(L)).

list_to_hex(L) ->
  lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(X) when X < 256 ->
  [hex(X div 16), hex(X rem 16)].

hex(N) when N < 10 ->
  $0+N;
hex(N) when N >= 10, N < 16 ->
  $A+(N-10).

