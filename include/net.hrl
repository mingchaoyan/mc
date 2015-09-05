
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, 
                      {reuseaddr, true}, {nodelay, false}, 
                      {delay_send, true}, {send_timeout, 50000}, 
                      {keepalive, true}, {exit_on_close, true}
                     ]).
-define(ACCEPTER_COUNT, 5).
-define(TCP_TIMEOUT, 1000). 
-define(HEADER_LENGTH, 2).
-define(HEART_TIMEOUT, 60 * 1000). %超时时间60s，超时发送{inet_async, Socket, Ref, {error, timeout}}消息
-define(HEART_TIMEOUT_MAX_COUNT, 4). % 最多超时次数
