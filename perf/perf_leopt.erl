-module(perf_leopt).
-export([start/0,start/1]).
-include_lib("liberl/include/utils.hrl").

start() -> start(20000).

start(Count) ->
   {Time,_}=timer:tc(fun() -> [ le:opt(debug) || _ <- lists:seq(1,Count) ] end),
   dsay("    ~6B opts, ~12.3f opts/sec, ~10.3f us/opt",
        [Count,Count*1000000/Time,Time/Count]).
