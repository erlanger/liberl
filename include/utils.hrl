%% This file needs to be included at the end of all the exports
%Format a string following io:format codes
-define(FMT(Fmt, Args), lists:flatten(io_lib:format(Fmt, Args))).

-export([dsay/2,dsay/3,dsay/4,
         say/2, say/3]).

say(Fmt,Args) when is_list(Fmt) ->
   dsay(true,Fmt,Args).

say(Level,Fmt,Args) when is_list(Fmt) ->
   dsay(le:opt(debug),Level,Fmt,Args).

%Debug commands
dsay(Fmt,Args) ->
   dsay(true,Fmt, Args).

dsay(true,Level,Fmt,Args) when is_integer(Level),
                               is_list(Fmt) ->
   dsay(1,Level,Fmt,Args);

dsay(false,_, _, _) ->
   ok;

dsay(CurrentLevel,Level,Fmt,_Args) when is_integer(Level),
                                       is_integer(CurrentLevel),
                                       is_list(Fmt),
                                       Level >  CurrentLevel ->
   ok;

dsay(undefined,Level,Fmt,Args) when is_integer(Level),
                                       is_list(Fmt) ->
   dsay( true, Fmt, Args);

dsay(CurrentLevel,Level,Fmt,Args) when is_integer(Level),
                                       is_integer(CurrentLevel),
                                       is_list(Fmt),
                                       Level =< CurrentLevel ->
   dsay( true, Fmt, Args).

dsay(false, _, _) ->
   ok;

dsay(0, _, _) ->
   ok;

dsay(Level,Fmt, Args) when is_integer(Level),
                           Level>0 ->
   dsay(true,Fmt, Args);

dsay(true, Fmt, Args) when is_list(Args)->
   TS = {_,_,Micro} = os:timestamp(),
   {{_Year,_Month,_Day},{H,M,S}} = calendar:now_to_universal_time(TS),
   Args1= [H,M,S+Micro/1000000,?MODULE|Args],
   io:format("\n\e[0m\e[32m~2.10.0B:~2.10.0B:~4.1.0f  \e[34;1m~w\e[0m: " ++ Fmt, Args1);

dsay(true, Fmt, Args) ->
   dsay(true,Fmt, lists:flatten([Args])).
