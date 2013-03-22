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
%%% Created : 21.03.2013
%%% -------------------------------------------------------------------
-module(sue_etop).

-include_lib("runtime_tools/include/observer_backend.hrl").
%% Application callbacks
-export([collect/0]).


collect() ->
    ProcInfo = etop_collect(processes(), []),
    #etop_info{now = now(),
           n_procs = length(ProcInfo),
           run_queue = erlang:statistics(run_queue),
           wall_clock = erlang:statistics(wall_clock),
           runtime = erlang:statistics(runtime),
           memi = etop_memi(),
           procinfo = ProcInfo
          }.

etop_memi() ->
    try
  [{total, c:memory(total)},
   {processes, c:memory(processes)},
   {ets, c:memory(ets)},
   {atom, c:memory(atom)},
   {code, c:memory(code)},
   {binary, c:memory(binary)}]
    catch
  error:notsup ->
      undefined
    end.

etop_collect([P|Ps], Acc) when P =:= self() ->
    etop_collect(Ps, Acc);
etop_collect([P|Ps], Acc) ->
    Fs = [registered_name,initial_call,memory,reductions,current_function,message_queue_len],
    case process_info(P, Fs) of
  undefined ->
      etop_collect(Ps, Acc);
  [{registered_name,Reg},{initial_call,Initial},{memory,Mem},
   {reductions,Reds},{current_function,Current},{message_queue_len,Qlen}] ->
      Name = case Reg of
           [] -> Initial;
           _ -> Reg
       end,
      Info = #etop_proc_info{pid=P,mem=Mem,reds=Reds,name=Name,
           cf=Current,mq=Qlen},
      etop_collect(Ps, [Info|Acc])
    end;
etop_collect([], Acc) -> Acc.


%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.