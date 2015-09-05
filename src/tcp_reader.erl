%%%-----------------------------------------------------
%%% @author mingchaoyan
%%% @copyright 2013 4399
%%% @doc
%%%
%%% @end
%%%--------------------------------------------------------------
-module(tcp_reader).

%%--------------------------------------------------------------
%% External export
%%--------------------------------------------------------------
-export([start_link/0, 
         init/0
        ]).

%%--------------------------------------------------------------
%% Internal export
%%--------------------------------------------------------------

%%--------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------
-include("net.hrl").
%%--------------------------------------------------------------
%% Records
%%--------------------------------------------------------------
-record(client, {
        player_pid,
        login  = 0,
        id  = 0,
        accname,
        timeout_count = 0 
        }).

%%--------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------

%%
%% API Functions
%%
start_link() ->
    {ok, proc_lib:spawn_link(?MODULE, init, [])}.

%%
%% Local Functions
%%
init() ->
    process_flag(trap_exit, true),
    Client = #client{player_pid = undefined,
                     login = 0,
                     accname = undefined,
                     timeout_count = 0},
    receive
        {go, Socket} ->
            put(socket, Socket),
            parse(Socket, Client);
        Other->
            lager:error("unknown message ~p", [Other])
    end.

handle_broadcast(Bin) ->
    Socket = get(socket),
    TotalLen=?HEADER_LENGTH+byte_size(Bin),
    BinToSend = <<TotalLen:16, Bin/binary>>,
    gen_tcp:send(Socket, BinToSend).

parse(Socket, Client) ->
    Ref = async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
    lager:debug("Ref:~p", [Ref]),
    receive
        {broadcast, Bin} ->
            handle_broadcast(Bin),
            parse(Socket, Client);
        {inet_async, Socket, Ref, {ok, <<TotleLen:16>>}} ->
            lager:info("%%%%%recv a new message with totle length:~p%%%%", [TotleLen]),
            BodyLen = TotleLen - ?HEADER_LENGTH,
            case BodyLen >= 0 of
                true ->
                    handle_parse_recv(Socket, BodyLen, Client);
                false ->
                    lager:error("Error! body < 0 ?")
            end,
            parse(Socket, Client);
        {inet_async, Socket, Ref, {error, timeout}} ->
            case Client#client.timeout_count >= ?HEART_TIMEOUT_MAX_COUNT of
                true ->
                    close_conn(Socket);
                false ->
                    lager:info("timeout for tcp receiving, (~p) times~n", [Client#client.timeout_count]),
                    parse(Socket, Client#client{timeout_count = Client#client.timeout_count + 1})
            end;
        {inet_async, Socket, Ref, {error, etimedout}} ->
            lager:info("error, etimedout"),
            close_conn(Socket);
        {'EXIT', From, Reason} ->
            lager:error("EXIT!, From:~p, Reason:~p", [From, Reason]);
        {inet_async, Socket, Ref, {error, closed}} ->
            ok;
        Other ->
            lager:info("Reason:~p", [Other])
    end.

handle_parse_recv(Socket, BodyLen, Client) ->
    case BodyLen == 0 of
        true ->
            handle_no_body(Socket, Client);
        false ->
            handle_body(Socket, BodyLen, Client)
    end.

handle_no_body(_Socket, _Client) ->
    lager:info("no body data?").

handle_body(Socket, BodyLen, _Client) ->
    RefData = async_recv(Socket, BodyLen, ?TCP_TIMEOUT),
    receive 
        {inet_async, Socket, RefData, {ok, Bin}} ->
            lager:debug("data from client:~p", [binary_to_list(Bin)]),
            ChildList = supervisor:which_children(tcp_reader_sup),
            lager:debug("ChildList:~p", [ChildList]),
            [Child ! {broadcast, Bin} || {_, Child, _, _} <- ChildList];
        Other ->
            lager:error("Error! Reason:~p", [Other])
    end.


async_recv(Sock, Length, Timeout) when is_port(Sock) ->
    case prim_inet:async_recv(Sock, Length, Timeout) of
        {error, Reason} -> Reason;
        {ok, Res}       -> Res
    end.

%%关闭连接(仅超时和下线时关闭)
close_conn(Socket) ->
    lager:info("Timeout! Close TCP!"),
    gen_tcp:close(Socket).

