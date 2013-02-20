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
-module(sender).

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
-record(state, {socket, port}).
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
handle_info(timeout, _State) ->
	{ok, Socket} = gen_udp:open(get_env(multi_port), ?OPTIONS),	
	inet:setopts(Socket ,[{add_membership,{get_env(multi_ip), ip_device:get_ip()}}]),
	{ok, {Address, Port}} = inet:sockname(Socket),
	io:format("1...Address : ~p  Port : ~p~n", [Address, Port]),	
	start_timer(),
	{noreply, #state{socket = Socket, port = Port}};
	
handle_info(send_alive, State=#state{socket = Socket, port = Port}) ->
	error_logger:info_msg("i am alive~n"),
	ok = gen_udp:send(Socket, ip_device:get_ip(),  Port, get_search()),
	start_timer(),
	{noreply, State};
	
handle_info({udp, Socket, IPtuple, InPortNo, Packet}, State) ->
	error_logger:info_msg("~n~nFrom IP: ~p~nPort: ~p~nData: ~p~n", [IPtuple, InPortNo, Packet]),
	 {noreply, State};
	 

handle_info(_Info, State) ->
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
start_timer() ->
	erlang:send_after(3000, self(), send_alive).

get_search() ->
	erlang:list_to_binary([<<"SEARCH:">>, get_cookie(), <<":">>, get_node(), <<":">>, get_state()]).
	
get_cookie() ->
	encode_cookie(atom_to_list(erlang:get_cookie())).

get_node() ->
	erlang:atom_to_binary(node(), utf8).	

get_state() ->
	<<"ACTIV">>.
		
encode_cookie(Cookie) ->
	crypto:md5_mac(Cookie, Cookie).
	
decode_cookies(Cookies, Cookie, []) ->
	decode_cookie(Cookies, Cookie, []).
	
decode_cookie([], Cookie, Acc) ->
	Acc;
decode_cookie([H|T], Cookie, Acc) ->
	case compare_cookie(H, Cookie) of
		true -> [Cookie|Acc];
		false -> Acc
	end. 
	
compare_cookie(H, Cookie) ->
	H =:= encode_cookie(Cookie).

get_env(Key) ->
	{ok, Value} = application:get_env(searcher, multi_port),
	Value.
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
get_env_test() ->
	application:load(searcher),
	?assertEqual(1900,get_env(multi_port)).
	
get_cookie_test() ->
	crypto:start(),
	?assertEqual(["nocookie"], decode_cookies([encode_cookie("nocookie")], "nocookie", [])).
	
-endif.
