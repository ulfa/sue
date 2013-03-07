%% Copyright 2012 Ulf Angermann
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 23.11.2012
%%% -------------------------------------------------------------------
-module(node_sup).
-behaviour(supervisor).
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0, start_child/1]).
-export([init/1]).
-define(LHS(Node),{node, {node, start_link, [Node]}, transient, brutal_kill, worker, [node]}).
-define(LHS(),{node, {node, start_link, []}, transient, brutal_kill, worker, [node]}).
%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Node) ->
	supervisor:start_child(node_sup, [Node]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->		
	RestartStrategy = {simple_one_for_one, 1, 3600},
    {ok, {RestartStrategy, [?LHS()]}}.
