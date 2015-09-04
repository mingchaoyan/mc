%% Author: mingchaoyan
%% Created: 2013-10-08
%% Description: Add description to tcp_reader_sup
-module(tcp_reader_sup).
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
    RestartStrategy = {simple_one_for_one, 10, 10},
    Element = {tcp_reader, {tcp_reader, start_link, []},
               temporary, brutal_kill, worker, [tcp_reader]}, 
    Children = [Element],
    {ok, {RestartStrategy, Children}}. 
