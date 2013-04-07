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
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/2, start/1]).

-export([get_status/1, sys_info/1, etop/1, memory/1, set_alive/1, pid_info/2]).
-export([get_applications/1, get_app_info/2]). 

%% ====================================================================
%% External functions
%% ====================================================================
get_app_info(Node, App) ->
	gen_server:call(Node, {app_info, Node, App}).

get_applications(Node) ->
	gen_server:call(Node, {applications, Node}).

set_alive(Node) when is_binary(Node)->
	set_alive(binary_to_atom(Node, utf8));
set_alive(Node) when is_atom(Node)->
	gen_server:cast(Node, set_alive).
	
memory(Node) ->
	memory1(Node).

sys_info(Node) ->
	sys_info1(Node).	
	
etop(Node) when is_atom(Node)->
	gen_server:call(Node, {etop, Node}).

pid_info(Node, Pid) when is_atom(Node)->
	gen_server:call(Node, {pid_info, Node, Pid}).
	
get_status(Node) when is_pid(Node)->
	gen_server:call(Node, get_state);
	
get_status(Node) when is_atom(Node)->
	gen_server:call(Node, get_state).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {status = ?UNKNOWN, node, time, ip, reason=[]}).
%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Node, Ip) ->
    gen_server:start_link({local, Node}, ?MODULE, [Node, Ip], []).
	
start([Node, Ip]) ->
	start_link(Node, Ip).	
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Node, Ip]) ->
	net_kernel:monitor_nodes(true, [nodedown_reason]),		
	start_timer(Node),
    {ok, #state{node = erlang:atom_to_binary(Node, utf8), ip = Ip, time = get_timestamp()}}.
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
handle_call(get_state, From, #state{status = Status, ip = Ip, time = Time, node = Node, reason = Reason} = State) -> 	
    {reply, {Node, [{ip, ip_device:ip_as_string(Ip)}, {state, Status}, {time, date:timestamp_to_date(Time)}, {reason, Reason}]}, State};

handle_call({app_info, Node, App}, From, State) ->
	Reply = get_app_info1(Node, App),
	{reply, Reply, State};

handle_call({applications, Node}, From, State) ->
	Reply = get_applications1(Node),
	{reply, Reply, State};

handle_call({etop, Node}, From, State) ->
	Reply = etop1(Node),
	{reply, Reply, State};

handle_call({pid_info, Node, Pid}, From, State) ->
	Reply = pid_info1(Node, Pid),
	{reply, Reply, State};	
handle_call(get_name, From, #state{node = Node} = State) ->
    {reply, Node, State};
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
handle_cast(set_alive, State) ->
	{noreply, State#state{status=?ALIVE, time=get_timestamp(), reason=[]}};
	
handle_cast(Msg, State) ->
    {noreply, State}.
%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({update, Node}, #state{status=Old_s}=State) ->
	%%error_logger:info_msg("........ update : ~p~n", [Node]),
	New_s = ping_node(Node),
	start_timer(Node),
	case New_s =:= Old_s of 
		true -> {noreply, State};
		false -> {noreply, State#state{status=New_s, time=get_timestamp(), reason=[]}}
	end;
	
handle_info({nodeup, Node, InfoList}, #state{node = Node1} = State) ->
	error_logger:info_msg("nodeup : ~p, ~p ~p ~n", [Node, Node1, InfoList]),
	case erlang:atom_to_binary(Node, latin1) =:= Node1 of
		true -> {noreply, State#state{status=?ALIVE, reason=InfoList, time=get_timestamp()}};
		false -> {noreply, State}
	end;
handle_info({nodedown, Node, InfoList}, #state{node = Node1} = State) ->
	error_logger:info_msg("nodedown : ~p, ~p ~p ~n", [Node, Node1, InfoList]),
	case erlang:atom_to_binary(Node, latin1) =:= Node1 of
		true -> {noreply, State#state{status=?DEAD, reason=InfoList, time=get_timestamp()}};
		false -> {noreply, State}
	end;

handle_info(Info, State) ->
	error_logger:info_msg(".....~p~n", [Info]),
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
get_app_info1(Node, App) ->
	process_info:start(),
	Processes = process_info:get_processes(App, all, Node),
	io:format("~p~n", [Processes]),
	convert_children(Processes).

get_applications1(Node) ->
	process_info:start(),
	Apps = process_info:get_applications(Node),
	[{Node, ''}|[{X, Node} || X <- Apps]].

etop1(Node) ->
	case rpc:call(Node, sue_etop, collect, []) of
		{badrpc,nodedown} -> [];
		Any -> Any
	end. 

pid_info1(Node, Pid) ->
	case rpc:call(Node, sue_etop, pid_info, [Pid]) of
		{badrpc,nodedown} -> [];
		Any -> Any
	end. 

sys_info1(Node) ->
	case rpc:call(Node, observer_backend, sys_info, []) of
		{badrpc,nodedown} -> [];
		Any -> Any
	end. 
	
memory1(Node) ->
	rpc:call(Node, erlang, memory, []).
	
start_timer(Node) ->
	erlang:send_after(10000, whereis(Node), {update, Node}).		
	
ping_node(Node) when is_binary(Node) ->
	ping_node(erlang:binary_to_atom(Node, utf8));
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


convert_children(unknown) ->
	[];
convert_children({{Parent, Children, []}, Num}) ->	
	[[get_name(Parent), '']|convert_children({Parent, Children, []}, [])];
convert_children({{Parent, Children, []}, Num}) ->
	[[get_name(Parent), '']|convert_children({Parent, Children, []}, [])].

convert_children({Parent, [], []}, Acc) ->
	Acc;

convert_children({Parent, [{Name, Children, []}|T], []}, Acc) ->		
	List = convert_children({Name, Children, []}, []),	
	Acc1 = lists:append(List, Acc), 
	convert_children({Parent, T, []}, [[get_name(Name),get_name(Parent)]|Acc1]).

get_name(Name_pid) ->
	Name = lists:nth(1,string:tokens(Name_pid, ":")),
	string:strip(Name, both).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
convert_app_info_test() ->
A={{"<0.77.0>",
  [{"tranceiver_sup : <0.80.0>",
    [{"sue_sup : <0.79.0>", [{"node_sup : <0.82.0>",[{"sue@kiezkantine : <0.129.0>",[],[]},{"moni@ua-TA880GB : <0.142.0>",[],[]}],[]}, {"<0.78.0>",[],[]}],[]},
     {"tranceiver : <0.81.0>", [{"Port :#Port<0.2339>",[],[]},{"Port :#Port<0.2325>",[],[]}],[]}],[]}
  ],[]},
 53277}.


 convert_children_test() ->
 	A= {{"<0.77.0>", [{"tranceiver_sup : <0.80.0>",  [{"sue_sup : <0.79.0>",  [{"node_sup : <0.82.0>",[{"sue@kiezkantine : <0.129.0>",[],[]},
 																{"moni@ua-TA880GB : <0.142.0>",[],[]}],[]}, {"<0.78.0>",[],[]}],[]}], []}],[]},53277},
 	?assertEqual([["<0.77.0>",''],["tranceiver_sup","<0.77.0>"],["sue_sup", "tranceiver_sup"], ["<0.78.0>","sue_sup"], ["node_sup", "sue_sup"], ["moni@ua-TA880GB", "node_sup"], ["sue@kiezkantine", "node_sup"]], convert_children(A)).

convert_lager_test() ->
	A=[{"<0.47.0>",
          [{"lager_event : <0.52.0>",
                      [{"lager_sup : <0.49.0>",
                        [{"lager_handler_watcher_sup : <0.53.0>",
                          [{"<0.55.0>",[],[]},
                           {"<0.57.0>",[],[]},
                           {"<0.58.0>",[],[]},
                           {"<0.56.0>",[],[]}],
                          []},
                         {"lager_crash_log : <0.54.0>",
                          [{"Port :#Port<0.1538>",[],[]}],
                          []},
                         {"<0.48.0>",[],[]}],
                        []},
                       {"Port :#Port<0.1575>",[],[]},
                       {"Port :#Port<0.1574>",[],[]}],
                      ["<0.55.0>","<0.56.0>","<0.57.0>"]}],
                    []},
                   []],
     ?assertEqual([], convert_children(A)).
-endif.


