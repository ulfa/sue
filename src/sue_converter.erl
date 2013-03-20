-module(sue_converter).

-include_lib("runtime_tools/include/observer_backend.hrl").

-export([record_to_proplist/1, recordlist_to_proplist/2]).


recordlist_to_proplist([], Acc) ->
	Acc;
recordlist_to_proplist([H|T], Acc) ->
	recordlist_to_proplist(T, [record_to_proplist(H)|Acc]).

record_to_proplist(#etop_info{} = Rec) ->
  lists:zip(record_info(fields, etop_info), tl(tuple_to_list(Rec)));	
  
record_to_proplist(#etop_proc_info{} = Rec) ->
  lists:zip(record_info(fields, etop_proc_info), tl(tuple_to_list(Rec))).

  
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).


record_to_proplist_list_test() ->
	A=[#etop_proc_info{}, #etop_proc_info{}],
	?assertEqual(2,erlang:length(recordlist_to_proplist(A, []))).
	
record_to_proplist_test() ->
	A = #etop_info{},
	?assertEqual([{now,{0,0,0}},
                  {n_procs,0},
                  {wall_clock,{0,0}},
                  {runtime,{0,0}},
                  {run_queue,0},
                  {alloc_areas,[]},
				  {memi, [{total, 0},
				  		 {processes, 0},
						 {ets, 0},
						 {atom, 0},
						 {code, 0},
     					 {binary, 0}]},
				  {procinfo,[]}
				  ], record_to_proplist(A)),
	B = #etop_proc_info{},
	?assertEqual([{pid,undefined},
				 {mem,0},
				 {reds,0},
				 {name,undefined},
				 {runtime,0},
				 {cf,undefined},
				 {mq,0}], record_to_proplist(B)).

-endif.