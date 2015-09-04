-module(tcp_listener_sup).
-behaviour(supervisor).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start_link/0, init/1]).

%%
%% API Functions
%%
start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

%%
%% Local Functions
%%

init([]) ->
    RestartStrategyAndMaximumRestartFrequency = {one_for_all, 10, 10},
    TcpAccepterSup = {tcp_accepter_sup,{tcp_accepter_sup, start_link, []},
                      transient, infinity, supervisor, [tcp_accepter_sup]},
    TcpListener = {tcp_listener,{tcp_listener, start_link, []},
                   transient, 100, worker, [tcp_listener]},
    Children = [TcpAccepterSup, TcpListener],
    {ok, {RestartStrategyAndMaximumRestartFrequency, Children}}. 
