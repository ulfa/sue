%% Copyright 2010 Ulf Angermann
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
%%% Created : 16.02.2012
%%% -------------------------------------------------------------------
%% @doc API to start and stop the application and to interact with

-module(sue).

%% Application callbacks
-export([start/0, stop/0]).
-export([get_children/0, get_children/1, add_node/1, sys_info/1, etop/1, memory/1]).
-export([get_applications/1, process_info/2, app_info/2]).

%% doc starrt the application
%%
	start() ->
		%%application:start(lager),		
	  	application:start(?MODULE).

%% doc stop the application
	stop() ->
		%%application:stop(lager),		
		application:stop(?MODULE).

%% doc Return all registered children. 
%% 
%% A child is Node which was registered throw the transceiver module
	get_children() ->
		node_sup:get_children().

%% doc Return the node with the name node
-spec get_children(atom()) -> node().

	get_children(Node) ->
		case rpc:call(Node, node_sup, get_children, []) of
			{badrpc,nodedown} -> [];
			Any -> Any
		end.
%% doc adds a node to the list of the nodes
-spec add_node(atom()) -> {ok, Child :: pid()}.			
	add_node(Node) when is_atom(Node) ->
		node_sup:start_child([Node, {0,0,0,0}]).
	
%% doc returns the sys_info of an node
-spec sys_info(atom()) -> [{atom(), any()}].
	sys_info(Node) ->
		lists:keysort(1,node:sys_info(Node)).
	
	etop(Node) ->
		node:etop(Node).

	process_info(Node, Pid) ->
		node:pid_info(Node, Pid).

	memory(Node) ->
		node:memory(Node).

	get_applications(Node) when is_atom(Node)->
		node:get_applications(Node). 

	app_info(Node, App) ->
		process_info:start(),
		[].