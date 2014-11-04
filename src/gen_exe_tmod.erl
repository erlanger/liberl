-module(gen_exe_tmod).
-behaviour(gen_exe).
-include_lib("common_test/include/ct.hrl").
-export([init/1,port_start/3,port_exit/2,state/0,state/1]).

-include_lib("liberl/include/utils.hrl").
init(Arg) ->
   dsay("Init called",[]),
   application:ensure_started(gproc),
   gproc:reg({p,l,state}),
   {ok,state([{init,[Arg]}])}.

port_start(When,Info,State) ->
   {ok,state([ {When,[Info]} |State])}.

   
port_exit(Info,State) ->
   gproc:reg({p,n,gen_exe_test_finished}), %this is only for the test SUITE!!
   {ok, state([ {port_exit,[Info]} |State])}.

state(S) ->
   gproc:set_value({p,l,state},S),
   S.

state() ->
   gproc:get_value({p,l,state}).
