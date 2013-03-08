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
%%% Created : 
%%% -------------------------------------------------------------------
-module(node).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/sue.hrl").
-include_lib("runtime_tools/include/observer_backend.hrl").
%% --------------------------------------------------------------------
%% External exports
-define(MAX_QUEUE_LENGTH, 19).
-define (TIMER, 30000).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1, start/1]).

-export([get_status/1]).

%% ====================================================================
%% External functions
%% ====================================================================
get_status(Node) when is_pid(Node)->
	gen_server:call(Node, get_state);
	
get_status(Node) when is_atom(Node)->
	gen_server:call(Node, get_state).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {status = ?UNKNOWN, node, time, ip = {0,0,0,0}}).
%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Node) ->
    gen_server:start_link({local, Node}, ?MODULE, [Node], []).
	
start(Node) ->
	start_link(Node).	
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Node]) ->
	net_kernel:monitor_nodes(true, [nodedown_reason]),	
	gen_server:cast(self(), {init_phase_2, Node}),
    {ok, #state{node = erlang:atom_to_binary(Node, latin1), time = get_timestamp()}}.
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(get_state, From, #state{status = Status, ip = Ip, time = Time, node = Node} = State) ->	
    {reply, {Node, [{ip, Ip}, {state, Status}, {time, Time}]}, State};

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.
%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({init_phase_2, Node}, State) ->
	error_logger:info_msg(".init_phase2~n"),
	{noreply, State#state{status=ping_node(Node)}};

handle_cast(Msg, State) ->
    {noreply, State}.
%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({nodeup, Node, InfoList}, #state{node = Node1} = State) ->
	error_logger:info_msg("........ nodeup : ~p, ~p ~p ~n", [Node, Node1, InfoList]),
	case erlang:atom_to_binary(Node, latin1) =:= Node1 of
		true -> {noreply, State#state{status=?ALIVE}};
		false -> {noreply, State}
	end;
handle_info({nodedown, Node, InfoList}, #state{node = Node1} = State) ->
	error_logger:info_msg(".........nodedown : ~p, ~p ~p ~n", [Node, Node1, InfoList]),
	case erlang:atom_to_binary(Node, latin1) =:= Node1 of
		true -> {noreply, State#state{status=?DEAD}};
		false -> {noreply, State}
	end;

handle_info(Info, State) ->
    {noreply, State}.
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------	
ping_node(Node) when is_binary(Node) ->
	ping_node(erlang:binary_to_atom(Node, latin1));
ping_node(Node)  ->
	case net_adm:ping(Node) of
		pang -> get_state(pang); 
		pong -> get_state(pong)
	end.
	
get_state(pang) ->
	?DEAD;
get_state(pong) ->
	?ALIVE.

get_timestamp() ->
    date:get_timestamp().

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.


