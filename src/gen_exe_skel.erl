-module(gen_exe_skel).
-behaviour(gen_exe).

-export([init/1,port_start/3,port_exit/2,port_data/4]).

-include_lib("liberl/include/utils.hrl").

init(Arg) ->
   application:ensure_started(gproc),
   application:ensure_started(liberl),
   {ok,#{mykey=>myvalue}}.

port_start(When,Info,State) ->
   {ok,State}.

   
port_exit(Info,State) ->
   {ok, State}.

port_data(Type,Data,Info,State) ->
   {ok, State}.
