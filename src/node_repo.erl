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
-module(node_repo).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/searcher.hrl").
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([start/0]).
-export([get_store/0, add_node/1, is_alive/1]).
%% ====================================================================
%% External functions
%% ====================================================================
get_store() ->
	gen_server:call(?MODULE, get_store).

add_node([{node, Node}, {state, State1}, {time, Time}, {ip, Ip}]) ->
	gen_server:cast(?MODULE, {save, [{node, Node}, {state, State1}, {time, Time}, {ip, Ip}]});

add_node(Node) ->
    gen_server:cast(?MODULE, {save, Node}).

%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {store=[]}).
%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
	start_link().
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

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
handle_call(get_store, From, #state{store = Store} = State) ->
    {reply, Store, State};
	
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
handle_cast({save, [{node, Node}, {state, State1}, {time, Time}, {ip, Ip}]}, #state{store = Store} = State) ->
	error_logger:info_msg("save node : ~p in state ~p with time : ~p~n", [Node, State1, Time]),		
	New_store = add([{node, Node}, {state, State1}, {time, Time}, {ip, Ip}], Store),
    {noreply, State#state{store = New_store}};

handle_cast({save, Node}, #state{store = Store} = State) ->
    error_logger:info_msg("save node : ~p", [Node]),
    New_store = add([{node, Node}, {state, ?DEAD}, {time, get_timestamp()}, {ip, {0,0,0,0}}], Store),
    {noreply, State#state{store = New_store}};

handle_cast(Msg, State) ->
	error_logger:info_msg("got unknown message : ~p~n", [Msg]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
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
%%keyreplace(Key, N, TupleList1, NewTuple) -> TupleList2
add([{node, Node}, {state, State1}, {time, Time}, {ip, Ip}], Store) ->
	lists:keystore(Node, 1, Store, {Node, [{ip, Ip}, {state, State1}, {time, Time}]}).

get_state([], Acc) ->
	Acc;
get_state([Node|Nodes], Acc) ->
	ok.
	
is_alive(pong) ->
	true;
is_alive(pang) ->
	false;
is_alive(Node) ->
	is_alive(net_adm:ping(Node)).

get_timestamp() ->
    date:get_timestamp().

ip_to_string(Ip) ->
	ip_device:ip_as_string(Ip).
	
timestamp_to_date(Time) when is_binary(Time) ->
	date:get_formated_date(calendar:gregorian_seconds_to_datetime(erlang:list_to_integer(erlang:binary_to_list(Time)))).

get_store_data([], Acc) ->
	Acc;
get_store_data([{Node, [{ip, Ip}, {state, State1}, {time, Time}]}|T], Acc) ->
	get_store_data(T, [[{Node, [{ip, ip_to_string(Ip)}, {state, State1}, {time, timestamp_to_date(Time)}]}]|Acc]).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
timestamp_to_date_test() ->
	?assertEqual("2013-03-01 13:52:24", timestamp_to_date(erlang:list_to_binary("63529365144"))).

get_store_data_test() ->
	A=[{"Node1", [{ip, {192,168,1,1}}, {state, "Dead"}, {time, erlang:list_to_binary("63529365144")}]}, 
	{"Node2", [{ip, {192,168,1,2}}, {state, "Activ"}, {time, erlang:list_to_binary("63529365144")}]}],
	B=get_store_data(A, []),
	?assertEqual(A,B).
add_test() ->
	?assertEqual([{"node", [{ip, "Ip"}, {state, "aktiv"}, {time, "time"}]}], add([{node, "node"}, {state, "aktiv"}, {time, "time"}, {ip, "Ip"}], [])).
-endif.
