%% common_test suite for gen_exe

-module(gen_exe_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("test_server/include/test_server_line.hrl").

-compile(export_all).

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%--------------------------------------------------------------------
suite() -> [{timetrap, {seconds, 20}}].

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% Description: Returns a list of test case group definitions.
%%--------------------------------------------------------------------
groups() -> [].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%%
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%
%%      NB: By default, we export all 1-arity user defined functions
%%--------------------------------------------------------------------
all() ->
    [ {exports, Functions} | _ ] = ?MODULE:module_info(),
    [ FName || {FName, _} <- lists:filter(
                               fun ({module_info,_}) -> false;
                                   ({all,_}) -> false;
                                   ({init_per_suite,1}) -> false;
                                   ({end_per_suite,1}) -> false;
                                   ({_,1}) -> true;
                                   ({_,_}) -> false
                               end, Functions)].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Description: Initialization before the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
   application:start(liberl),
   ok=meck:new(tmod, [non_strict,no_link]),

   ok=meck:expect(tmod,init,
         fun(TestPid) ->
               {ok,#{ testpid=>TestPid,
                      calls=>[{init,TestPid}]
                    } }
         end),

   ok=meck:expect(tmod,port_start,
         fun(When,Info,State) ->
               {ok,tmod:state({When,Info},State) }
         end),

   ok=meck:expect(tmod,port_data,
         fun (_Type,{sum,Sum},_Info,State=#{testpid:=TestPid}) ->
               TestPid ! {sum,Sum,State},
               {ok,State};

             (_Type,Data,_Info,State) ->
               {ok,tmod:state({port_data,Data},State) }
         end),

   ok=meck:expect(tmod,port_exit,
         fun (Info,State=#{testpid:=TestPid}) ->
               S1=tmod:state({port_exit,Info},State),
               TestPid ! {exit,Info,S1},
               {ok, S1}
         end),

   ok=meck:expect(tmod,terminate,
         fun (_Reason) -> ok end),

   ok=meck:expect(tmod,state,
         fun (NewTerm,S=#{calls:=Calls}) ->
               S1=S#{calls:=[NewTerm|Calls]},
               S1
         end),

   ct:pal("meck tmod started"),

   eprof:start(),
   Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
   eprof:stop(),
   meck:unload(tmod),
   ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% Description: Initialization before each test case group.
%%--------------------------------------------------------------------
init_per_group(_group, Config) ->
   Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% Description: Cleanup after each test case group.
%%--------------------------------------------------------------------
end_per_group(_group, Config) ->
   Config.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Description: Initialization before each test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(TestCase, Config) ->
   eprof:start_profiling([self()]),
   Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for failing the test case.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(TestCase, Config) ->
   eprof:stop_profiling(),
   eprof:analyze(procs),
   Config.

callorder() ->
   [{userdata,[{doc,"Testing the gen_exe module"}]}].


callorder(_Config) ->
   % Test is done for both shell mode and non shell mode
   [ begin
    ExeSpec=#{ path=>["/usr/bin/echo"] },
    ?line {ok,Pid}=gen_exe:start_link(tmod,{ExeSpec,self()},Mode ++ [{debug,10}]),
    receive {exit,_Info,ModState} -> ok end,

    %Make sure callbacks were executed in order
    ct:pal("~p~nmodule state=~p",[Mode,ModState]),
    ?line [{port_exit,_},{post_start,_},{pre_start,_},{init,_}] = maps:get(calls,ModState),
    ?line true=meck:validate(tmod)
    end
    || Mode <- [[start],[start,shell]] ],
    {comment,"Good, callbacks running in order."}.

keep_alive(_Config) ->
   Count=50,
   CountMinusOne=Count-1,
   ExeSpec=#{ path=>["/usr/bin/echo"] },
   ?line {ok,Pid}=gen_exe:start_link(tmod,{ExeSpec,self()},[start,keep_alive,{debug,10}]),
   T1=now(),
   C1=[begin
       receive {exit,Info,_ModState} -> ok end,
       {C,Info}
       end || C <- lists:seq(1,Count) ],
       T2=now(),
       Diff=timer:now_diff(T2,T1)/1000,
       %Make sure port exited 50 times and also
       %that restarts_normal returns the right number
       {Count, #{restarts_normal:=CountMinusOne} } = lists:last(C1),

       R = sys:get_state(Pid),

       ct:pal("gen_exe state=~p",[R]),
       ?line true=meck:validate(tmod),
       Comment = io_lib:format("ok, ~B processes restarted; ~.3f ms/process",[Count,Diff/Count]),
       { comment, Comment }.

start_stop(_Config) ->
   ExeSpec=#{ path=>[{app,liberl},"c_src/le_eixx"] },

   ?line {ok,Pid}=gen_exe:start_link(tmod,{ExeSpec,self()},[{debug,10}]),
   gen_exe:port_start(Pid,[]),

   %Second call to port_start should error
   {error,{already_started,_}} = gen_exe:port_start(Pid,[]),

   gen_exe:port_stop(Pid,"Bye"),

   %Second call to port_stop should be okay
   ok=gen_exe:port_stop(Pid,"Bye"),
   receive {exit,_Info,ModState} -> ok end,

   %Make sure port exit status is 0 (le_eixx should have ended normally)
   ct:pal("module state=~p",[ModState]),
   ?line [{port_exit,#{status:=0} },{post_start,_},{pre_start,_},{init,_}] = maps:get(calls,ModState),
   ?line true=meck:validate(tmod),
   {comment,"Port start/stop working."}.

port_kill(_Config) ->
   ExeSpec=#{ path=>["cat"] },

   ?line {ok,Pid}=gen_exe:start_link(tmod,{ExeSpec,self()},[shell,start,{debug,10}]),
   {os_pid, OsPid}=erlang:port_info(gen_exe:get(Pid,port),os_pid),
   Pscmd=io_lib:format("ps -p ~w -o s=",[OsPid]),
   %Make sure process is running
   true = [] =/= os:cmd(Pscmd),
   gen_exe:port_stop(Pid,"Bye",2000),
   timer:sleep(200),
   %Make sure it is still running before
   % the kill timeout
   true = [] =/= os:cmd(Pscmd),
   %Make sure it is not running afer the
   %timeout
   timer:sleep(2200),
   [] = os:cmd(Pscmd),
   receive {exit,Info,_ModState} -> ok end,
   ct:pal("port_exit info=~p",[Info]),
   {comment,"Port kill timeout working"}.

port_data(_Config) ->
   ExeSpec=#{ path=>[{app,liberl},"c_src/le_eixx"] },

   ?line {ok,Pid}=gen_exe:start_link(tmod,{ExeSpec,self()},[start,{debug,10}]),
   Count=300,
   T1=now(),
   [ gen_exe:port_cast(Pid,{add,N}) || N<-lists:seq(1,Count) ],
   gen_exe:port_cast(Pid,getsum),

   %Wait for sum to get in
   Sum=receive {sum,Sum1,_ModSate} -> Sum1 end,
   T2=now(),

   %Make sure we received the sum properly calculated
   ct:pal("port_sum info=~p",[Sum]),
   true = Sum==Count*(Count+1)/2,

   Diff=timer:now_diff(T2,T1)/Count,
   gen_exe:port_stop(Pid,"Bye"),
   ?line true=meck:validate(tmod),
   Comment = io_lib:format("ok, ~B msgs sent; ~.3f ms/msg",[Count,Diff/Count]),
   { comment, Comment }.

%%TODO: Tests for module responses, for some reason the test cases don't crash
%%even if there is a bad_response (but the gen_exe server terminates properly)
