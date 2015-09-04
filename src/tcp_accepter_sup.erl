-module(tcp_accepter_sup).
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
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%
%% Local Functions
%%

init([]) ->
    RestartStrategyAndMaximunRestartFrequency = {simple_one_for_one, 10, 10},
    Element = {tcp_accepter, {tcp_accepter, start_link, []},
               transient, brutal_kill, worker, [tcp_accepter]},
    Children = [Element],
    {ok, {RestartStrategyAndMaximunRestartFrequency, Children}}. 
