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
-module(sue).

%% Application callbacks
-export([start/0, stop/0]).
-export([get_children/0, add_node/1, sys_info/1, etop/1]).

	start() ->		
	  application:start(?MODULE).

	stop() ->
		application:stop(?MODULE).

	get_children() ->
		node_sup:get_children().
		
	add_node(Node) when is_atom(Node) ->
		node_sup:start_child([Node, {0,0,0,0}]).
	
	sys_info(Node) ->
		node:sys_info(Node).
	
	etop(Node) ->
		node:etop(Node).
