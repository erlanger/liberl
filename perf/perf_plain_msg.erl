-module(perf_plain_msg).
-compile(export_all).

-include_lib("liberl/include/utils.hrl").

ping(Pid,Msg) -> Pid ! {self(),Msg}, receive R -> R end.
pong()        -> receive {From,Msg} -> From ! Msg ,pong() end.

start() ->
   start(10000).

start(Count) ->
   Pid=listen(),
   run(Pid,Count).

run(Pid,Count) ->
   {Time,_}=timer:tc(fun() -> [ ping(Pid,["../c_src/le_eixx"]) 
                               || _X <- lists:seq(1,Count) ] end),
   dsay("~6B msgs, ~12.3f msgs/sec, ~10.3f us/msg",
        [Count,Count*1000000/Time,Time/Count]).

listen() ->
   P=spawn(fun pong/0),
   erlang:register(pong,P),
   P.
