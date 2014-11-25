-module(perf_gen_server).
-behaviour(gen_server).

%% API.
-export([start/1,start/0,start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {cnt=0,n=0,port=undefined}).  

-include_lib("liberl/include/utils.hrl").
%% API.

start() -> start(10000).

start(Count) -> 
   start(Count,erlang,send),
   start(Count,gen_server,cast),
   start(Count,gen_server,call).

start(Count,M,F) ->
   {ok,Pid}=start_link(Count),
   T1=os:timestamp(),
   [ begin M:F(Pid,{self(),["../c_src/le_eixx"]}), rcv(F) end
    || _ <- lists:seq(1,Count) ],
   T2=os:timestamp(),
   Time=timer:now_diff(T2,T1),
   Objs=atom_to_list(F) ++ "s",
   say("~5B ~s, ~11.3f ~s/sec, ~9.3f us/~s",
       [Count,Objs,Count*1000000/Time,Objs,Time/Count,F]).

start_link(Count) ->
	gen_server:start_link(?MODULE, Count, []).

rcv(call) -> ok;

rcv(Type) ->
   receive finished -> ok;
           _        -> ok
   after 5000 -> say("~p Timeout",[Type]),timeout 
   end.
           
%% gen_server.

init(Count) ->
   %Port=open_port({spawn_executable,"../c_src/le_eixx"},
   %               [binary,{packet,4},use_stdio,hide]),
	%{ok, #state{cnt=Count,n=1,port=Port}}.
	{ok, #state{cnt=Count,n=1}}.


handle_call(_Request, {Pid,_Tag}, S=#state{n=N,cnt=Count} ) when N==Count->
   Pid ! finished,
	{stop, normal, ok,  S};

handle_call(Request, {_From,_Tag}, S=#state{n=N}) ->
	{reply, Request, S#state{n=N+1} }.


handle_cast({From,_Msg}, S=#state{n=N,cnt=Count} ) when N==Count->
   From ! finished,
	{stop, normal, S};

handle_cast({From,Msg}, S=#state{n=N,port=Port})  ->
   From ! Msg,
   %say("n=~p",N),
	{noreply, S#state{n=N+1}}.


handle_info({From,_Msg}, S=#state{n=N,cnt=Count} ) when N==Count->
   From ! finished,
	{stop, normal, S};

handle_info({From,Msg}, S=#state{n=N}) ->
   From ! Msg,
	{noreply, S#state{n=N+1}}.


terminate(_Reason, _State) ->
   %dsay("gs terminating",[]),
   ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
