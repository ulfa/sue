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
-module(tranceiver).

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

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
-record(state, {sender, receiver}).
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
    {ok, #state{}, 0}.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------	
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({udp, Socket, Ip, InPortNo, Packet}, State) ->
	%%error_logger:info_msg("~n~nFrom IP: ~p~nPort: ~p~nData: ~p~n", [Ip, InPortNo, Packet]),
	Node = decode_message(Packet),
	save_node(lists:append(Node,[{ip, Ip}])),	
	{noreply, State};

handle_info(timeout, _State) ->
	{ok, Sender} = gen_udp:open(0, ?SENDER_OPTIONS),	
	{ok, Receiver} = gen_udp:open(get_env(multi_port), ?RECEIVER_OPTIONS),	
	start_timer(),
	{noreply, #state{sender = Sender, receiver = Receiver}};
	
handle_info(send_alive, State=#state{sender = Socket}) ->
	{ok, {Address, Port}} = inet:sockname(Socket),
%%	error_logger:info_msg("1... IP : ~p  Port : ~p~n", [Address, Port]),
	ok = gen_udp:send(Socket, get_env(multi_ip),  get_env(multi_port), get_search()),
	start_timer(),		
	{noreply, State};
		  

handle_info(Info, State) ->
	error_logger:warning_msg("don't understand this message : ~p~n", [Info]),	
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% <<"SEARCH:COOKIENODE:STATE:TIME">>
decode_message(<<_Action:7/binary, Cookie:16/binary, Rest/binary>> = Message) ->	
	Is_valid_cookie = decode_cookie(Cookie, get_local_cookie()),
	[Node, R1] = binary:split(Rest,<<":">>),
	[State, Time] = binary:split(R1,<<":">>),
	[{valid, Is_valid_cookie},{node, Node}, {state, State}, {time, Time}].
	
decode_message(false, _Node, _State, _Time, Ip) ->	
	error_logger:info_msg("Cookie which was received is not a requested one~n"),
	ok.
	
save_node([{valid, true}, {node, Node}, {state, State}, {time, _Time}, {ip, Ip}]) ->
	%%error_logger:info_msg("save : ~p in state : ~p ~n", [Node, State]),
	gen_server:cast(get_env(node_repo), {get_env(node_repo_msg), [{node, Node}, {state, State}, {time, _Time}, {ip, Ip}]});
	
save_node([{valid, false}, {node, Node}, {state, State}, {time, _Time}, {ip, _Ip}]) ->
	error_logger:info_msg("don't save : ~p in state : ~p ~n", [Node, State]).
	
get_node_repo() ->
	Module = get_env(node_repo),
	Msg = get_env(node_repo_msg),
	[Module, Msg].
	
start_timer() ->
	erlang:send_after(get_env(timer), self(), send_alive).

get_search() ->
	erlang:list_to_binary([<<"SEARCH:">>, get_cookie(), get_node(), <<":">>, get_state(), <<":">>, get_timestamp()]).
	
get_local_cookie() ->
	atom_to_list(erlang:get_cookie()).
	
get_cookie() ->
	encode_cookie(atom_to_list(erlang:get_cookie())).

get_node() ->
	erlang:atom_to_binary(node(), utf8).	

get_state() ->
	<<"ACTIV">>.

get_timestamp() ->
	erlang:list_to_binary(erlang:integer_to_list(calendar:datetime_to_gregorian_seconds(calendar:local_time()))).
	
encode_cookie(Cookie) ->
	crypto:md5_mac(Cookie, Cookie).
		
decode_cookie(Cookie, Local_cookie) ->
	compare_cookie(Cookie, Local_cookie).
	
compare_cookie(Cookie, Local_cookie) ->
	Cookie =:= encode_cookie(Local_cookie).

get_env(Key) ->
	{ok, Value} = application:get_env(searcher, Key),
	Value.
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
encode_message_test() ->	
	?assertEqual([{valid, false}, {node, <<"Node">>}, {state, <<"State">>}, {time, <<"Time">>}],decode_message(<<"SEARCH:1234567890123456Node:State:Time">>)).

decode_cookie_test() ->
	crypto:start(),
	?assertEqual(true, decode_cookie(encode_cookie("cookie"), "cookie")),
	?assertEqual(false, decode_cookie(encode_cookie("cookie"), "cookie1")).
	
get_env_test() ->
	application:load(searcher),
	?assertEqual(1900,get_env(multi_port)).
		
-endif.
