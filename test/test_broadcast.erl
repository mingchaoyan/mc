-module(test_broadcast).
-export([start/0]).

%-define(HOST, "45,33.38.56").
-define(HOST, "127.0.0.1").
-define(PORT, 10000).

-include("net.hrl").
start() ->
    {ok, Socket} = gen_tcp:connect(?HOST, ?PORT, ?TCP_OPTIONS),
    Data = "hello world",
    DataBin = list_to_binary(Data),
    Len = byte_size(DataBin),
    TotalLen = Len + ?HEADER_LENGTH,
    Bin = <<TotalLen:16, DataBin/binary>>,
    ok = gen_tcp:send(Socket, Bin),
    receive_data(Socket).

receive_data(Socket) ->
    case gen_tcp:recv(Socket, ?HEADER_LENGTH) of
        {ok, <<TotalLen:16>>} ->
            lager:info("TotalLen:~p", [TotalLen]),
            BodyLen = TotalLen - ?HEADER_LENGTH,
            case BodyLen > 0 of
                true ->
                    case gen_tcp:recv(Socket, BodyLen) of
                        {ok, DataBin} ->
                            Data = binary_to_list(DataBin),
                            lager:info("from server:~p", [Data]);
                        false ->
                            ok
                    end;
                false ->
                    lager:error("no body?")
            end;
        _ ->
            ok
    end,
    receive_data(Socket).

