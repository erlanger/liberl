-module(perf_leopt).
-export([start/0,start/1]).
-include_lib("liberl/include/utils.hrl").

start() -> start(200).

start(Count) ->
   {Time,_}=timer:tc(fun() -> [ le:opt(debug) || _ <- lists:seq(1,Count) ] end),
   dsay("  ~6B calls, ~12.3f calls/sec, ~10.3f us/call",
        [Count,Count*1000000/Time,Time/Count]).
