-module(perf_portcmd).
-compile(export_all).

-include_lib("liberl/include/utils.hrl").

ping(Port,_Msg) -> port_command(Port,term_to_binary(getcmdline)), receive R -> R end.

start() ->
   start(10000).

start(Count) ->
   Port=open_port({spawn_executable,"../c_src/le_eixx"},
                  [binary,{packet,4},use_stdio,hide]),
   run(Port,Count),
   port_command(Port,term_to_binary({stop,"Last msg sent"})).

run(Port,Count) ->
   {Time,_}=timer:tc(fun() -> [ ping(Port,["../c_src/le_eixx"]) 
                               || _X <- lists:seq(1,Count) ] end),
   dsay("  ~6B msgs, ~12.3f msgs/sec, ~10.3f us/msg",
        [Count,Count*1000000/Time,Time/Count]).

