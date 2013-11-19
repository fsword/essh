-module(essh_receiver).

-export([handle/2]).

handle(Result, Timeout) ->
    io:format("loop start"),
    loop({none,<<>>}, Result, Timeout).

loop({Status,Data}, Result, Timeout) ->
    receive
        {data, AppendData} ->
            NewData = <<Data/binary, AppendData/binary>>,
            loop({Status, NewData}, Result, Timeout);
        {exit, NewStatus} ->
            io:format("loop exit"),
            loop({NewStatus,Data}, Result, Timeout);
        eof ->
            io:format("loop fin"),
            loop({Status,Data}, Result, Timeout);
        close ->
            {Status, Data}
    after
        Timeout -> Result
    end.
