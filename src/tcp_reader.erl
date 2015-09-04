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
-define(TCP_TIMEOUT, 1000). 
-define(HEADER_LENGTH, 2).
-define(HEART_TIMEOUT, 60 * 1000). %超时时间60s，超时发送{inet_async, Socket, Ref, {error, timeout}}消息
-define(HEART_TIMEOUT_MAX_COUNT, 4). % 最多超时次数

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
            %parse_login(Socket, Client);
            parse(Socket, Client);
        Other->
            lager:error("unknown message ~p", [Other])
    end.

%%parse_login(Socket, Client) ->
%%    Ref = async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
%%    receive
%%        {inet_async, Socket, Ref, {ok, <<TotleLen:16, Cmd:16, _IsCompressed:8>>}} ->
%%            lager:info("receive cmd ~p",[Cmd]),
%%            BodyLen = TotleLen - ?HEADER_LENGTH,
%%            case BodyLen >= 0 of
%%                true ->
%%                    handle_parse_login_recv(Socket, BodyLen, Client, Cmd);
%%                false ->
%%                    lager:error("Error! this pack has no body")
%%            end;
%%        {inet_async, Socket, Ref, {error, timeout}} ->
%%            case Client#client.timeout_count >= ?HEART_TIMEOUT_MAX_COUNT of
%%                true ->
%%                    close_conn(Socket);
%%                false ->
%%                    ?INFO(parse_login, "timeout for tcp receiving, (~p) times~n", [Client#client.timeout_count]),
%%                    parse_login(Socket, Client#client{timeout_count = Client#client.timeout_count + 1})
%%            end;
%%        {inet_async, Socket, Ref, {error, etimedout}} ->
%%            ?ERR(parse_login, "error, etimedout"),
%%            close_conn(Socket);
%%        Other ->
%%            ?INFO(parse_login, "Reason:~p", [Other])
%%    end.
%%
%%handle_parse_login_recv(Socket, BodyLen, Client, Cmd) ->
%%    RefData = async_recv(Socket, BodyLen, ?TCP_TIMEOUT),
%%    receive
%%        {inet_async, Socket, RefData, {ok, Bin}} ->
%%            case extract(Cmd, Bin) of
%%                {ok, login, Data} ->
%%                    handle_login(Socket, Client, Data);
%%                {ok, create, {ServerId, AccountName, Nickname, Gender}} ->
%%                    handle_create(Socket, Client, {ServerId, AccountName, Nickname, Gender})
%%            end;
%%        Other ->
%%            ?WARNING(parse_login, "Error! Reason:~p", [Other])
%%    end.
%%
%%handle_login(Socket, Client, Data) ->
%%    Reply = gen_server:call(login_server, {login, Data}),
%%    case Reply of
%%        Reply when Reply > 0 ->
%%            PlayerId = Reply,
%%            {ok, Pid} = mod_player:start_link({PlayerId, Socket}),
%%            login_server:add_online_player(PlayerId, Pid),
%%            mod_player:handle_login(PlayerId, Socket),
%%            Client1 = Client#client{
%%                    login = 1,
%%                    id = PlayerId,
%%                    player_pid = Pid,
%%                    timeout_count = 0
%%                    },
%%            BinData = pt_10:write(10000, <<?OPERATION_SUCCESS:16, PlayerId:64>>),
%%            gen_tcp:send(Socket, BinData),
%%            parse(Socket, Client1);
%%        _ ->
%%            BinData = pt_10:write(10000, <<Reply:16, 0:64>>),
%%            gen_tcp:send(Socket, BinData),
%%            parse_login(Socket, Client)
%%    end.
%%
%%handle_create(Socket, Client, {ServerId, AccountName, Nickname, Gender}) ->
%%    Result = lib_player_create:create({ServerId, AccountName, Nickname, Gender, Socket}),
%%    BinData = pt_10:write(10001, <<Result:16>>),
%%    gen_tcp:send(Socket, BinData),
%%    parse_login(Socket, Client).

parse(Socket, Client) ->
    Ref = async_recv(Socket, ?HEADER_LENGTH, ?HEART_TIMEOUT),
    receive
        {inet_async, Socket, Ref, {ok, <<TotleLen:16, Cmd:16, _IsCompressed:8>>}} ->
            lager:info("%%%%%recv a new Cmd [~p] with totle length:~p%%%%", [Cmd, TotleLen]),
            BodyLen = TotleLen - ?HEADER_LENGTH,
            case BodyLen >= 0 of
                true ->
                    handle_parse_recv(Socket, BodyLen, Client, Cmd);
                false ->
                    lager:error("Error! body < 0 ?")
            end;
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

handle_parse_recv(Socket, BodyLen, Client, Cmd) ->
    case BodyLen == 0 of
        true ->
            handle_no_body(Socket, Client, Cmd);
        false ->
            handle_have_body(Socket, BodyLen, Client, Cmd)
    end.

handle_no_body(Socket, Client, Cmd) ->
    case catch gen_server:call(Client#client.player_pid, {routing, Cmd, <<>>}) of
        ok ->
            parse(Socket, Client#client{timeout_count = 0});
        error -> 
            lager:error("player process down!continue to parse!"),
            parse(Socket, Client#client{timeout_count = 0});
        {'EXIT', Reason} ->
            lager:error("Error! Reason:~p", [Reason])
    end.

handle_have_body(Socket, BodyLen, Client, Cmd) ->
    RefData = async_recv(Socket, BodyLen, ?TCP_TIMEOUT),
    receive 
        {inet_async, Socket, RefData, {ok, Bin}} ->
            %% TODO 广播客户端
            case extract(Cmd, Bin) of 
                {ok, Data} ->
                    case catch gen_server:call(Client#client.player_pid, {routing, Cmd, Data}) of
                        ok ->
                            parse(Socket, Client#client{timeout_count = 0});
                        {'EXIT', Reason} ->
                            lager:error("Error! Reason:~p", [Reason])
                    end;
                {bad_arg, Arg} -> %% 协议格式ok，但数据不正常，忽略，但不断开连接
                    lager:warning("Bad arg:~p", [Arg]),
                    parse(Socket, Client#client{timeout_count = 0});
                ignore -> %% 协议格式ok，但协议数据不符合或该用户无权限执行GM指令，忽略，但不断开连接
                    lager:warning("No Privilege! Ignore GM CMD from:~p, User is:~p", 
                             [utils:get_player_ip(Client#client.id), Client#client.accname]),
                    %% gen_tcp:send(Socket, <<?GM_NO_PRIVILEGE:16/signed>>),
                    parse(Socket, Client#client{timeout_count = 0});
                Other -> %% 协议格式异常，认为是非正常客户端，断开连接
                    lager:error("Error! Disconnect Client! ~nReason:~p", [Other])
            end;
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

extract(Cmd, Binary) ->
    case integer_to_list(Cmd) of
        [H1, H2, _, _, _] ->
            Module = list_to_atom("pt_" ++ [H1, H2]),
            case catch Module:read(Cmd, Binary) of
                {'EXIT', Error} -> 
                    Error;
                Data ->
                    Data
            end;
        _ ->
            protocal_error
    end.  

%%handle_protocal_heart_beat(Cmd, Socket, Client) ->
%%    case Cmd =:= ?PROTOCAL_HEART_BEAT of
%%        true ->
%%            parse_login(Socket, Client);
%%        false ->
%%            ignore
%%    end.

