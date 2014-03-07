-module(essh_receiver).

-export([handle/2]).

handle(Result, Timeout) ->
    error_logger:info_msg("loop start"),
    loop({none,<<>>}, Result, Timeout).

loop({Status,Data}, Result, Timeout) ->
    receive
        {data, AppendData} ->
            NewData = <<Data/binary, AppendData/binary>>,
            loop({Status, NewData}, Result, Timeout);
        {exit, NewStatus} ->
            error_logger:info_msg("loop exit"),
            loop({NewStatus,Data}, Result, Timeout);
        eof ->
            error_logger:info_msg("loop fin"),
            loop({Status,Data}, Result, Timeout);
        close ->
            {ok, Status, Data}
    after
        Timeout -> {timeout, Result}
    end.
