%% common_test suite for gen_exe

-module(gen_exe_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("test_server/include/test_server_line.hrl").

-compile(export_all).
-define(STATFILE,"/tmp/liberl.stats").

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
suite() -> [{timetrap, {seconds, 50}}].

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
         fun (_Type,Data,_Info,State=#{testpid:=TestPid}) ->
               TestPid ! {data,Data,State},
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

   ok=meck:expect(tmod,handle_cast,
         fun(Req,State=#{testpid:=TestPid}) ->
               TestPid ! {cast,Req},
               {noreply,tmod:state({handle_cast,Req},State) }
         end),

   ok=meck:expect(tmod,handle_call,
         fun(Req={add,A,B},_From,State) ->
               {reply, A+B, tmod:state({handle_call,Req},State) }
         end),

   ok=meck:expect(tmod,handle_info,
         fun(Msg,State=#{testpid:=TestPid}) ->
               TestPid ! {info,Msg},
               {noreply,tmod:state({handle_info,Msg},State) }
         end),

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
init_per_testcase(_TestCase, Config) ->
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
end_per_testcase(_TestCase, Config) ->
   eprof:stop_profiling(),
   eprof:analyze(procs),
   Config.

callorder() ->
   [{userdata,[{doc,"Testing the gen_exe module"}]}].


callorder(_Config) ->
   % Test is done for both shell mode and non shell mode
   [ begin
       ExeSpec=#{ path=>["/bin/echo"] },
       ?line {ok,Pid}=gen_exe:start_link(tmod,{ExeSpec,self()},Mode ++ [{debug,10}]),
       ModState=receive {exit,_Info,State} -> State end,

       %Make sure callbacks were executed in order
       ct:pal("~p~nmodule state=~p",[Mode,ModState]),
       ?line [{port_exit,_},{post_start,_},{pre_start,_},{init,_}] = maps:get(calls,ModState),
       gen_exe:stop(Pid,normal),
       ?line true=meck:validate(tmod)
    end
    || Mode <- [[start],[start,shell]] ],
    {comment,"Good, callbacks running in order."}.

keep_alive(_Config) ->
   Count=50,
   CountMinusOne=Count-1,
   ExeSpec=#{ path=>["/bin/echo"] },
   ?line {ok,Pid}=gen_exe:start_link(tmod,{ExeSpec,self()},[start,keep_alive,{debug,10}]),
   T1=now(),
   C1=[begin
       Info=receive {exit,Info1,_ModState} -> Info1 end,
       {C,Info}
       end || C <- lists:seq(1,Count) ],
   T2=now(),
   Diff=timer:now_diff(T2,T1)/Count,
   %Make sure port exited 50 times and also
   %that restarts_normal returns the right number
   {Count, #{restarts_normal:=CountMinusOne} } = lists:last(C1),

   R = sys:get_state(Pid),

   ct:pal("gen_exe state=~p",[R]),
   ?line true=meck:validate(tmod),
   Comment = io_lib:format("ok, ~B processes restarted; ~.3f us/process~n",[Count,Diff]),
   file:write_file(?STATFILE,Comment,[append]),
   gen_exe:stop(Pid,normal),
   { comment, Comment }.

start_stop(_Config) ->
   ExeSpec=#{ path=>[{app,liberl},"c_src/le_eixx"] },

   ?line {ok,Pid}=gen_exe:start_link(tmod,{ExeSpec,self()},[{debug,10}]),
   %port_stop with no port should return error
   {error, port_not_started}=gen_exe:port_stop(Pid,"Bye"),
   gen_exe:port_start(Pid,[]),

   %Second call to port_start should error
   {error,{already_started,_}} = gen_exe:port_start(Pid,[]),

   gen_exe:port_stop(Pid,"Bye"),

   %Second call to port_stop should error
   {error, port_not_started}=gen_exe:port_stop(Pid,"Bye"),
   ModState=receive {exit,_Info,State} -> State end,

   %Make sure port exit status is 0 (le_eixx should have ended normally)
   ct:pal("module state=~p",[ModState]),
   ?line [{port_exit,#{status:=0} },{post_start,_},{pre_start,_},{init,_}] = maps:get(calls,ModState),
   ?line true=meck:validate(tmod),
   gen_exe:stop(Pid,normal),
   {comment,"Port start/stop working."}.

port_kill(_Config) ->
   ExeSpec=#{ path=>["cat"] },

   ?line {ok,Pid}=gen_exe:start_link(tmod,{ExeSpec,self()},[shell,start,{debug,10}]),
   {os_pid, OsPid}=erlang:port_info(gen_exe:get(Pid,port),os_pid),
   Pscmd=io_lib:format("ps -p ~w -o s=",[OsPid]),
   %Make sure process is running
   true = [] =/= os:cmd(Pscmd),
   gen_exe:port_stop(Pid,"Bye",2000),
   %Make sure it is not running afer the
   %timeout
   [] = os:cmd(Pscmd),
   %Make sure port_exit is called
   {ok,Info}=receive {exit,Info1,_ModState} -> {ok,Info1} end,
   ct:pal("port_exit info=~p",[Info]),
   gen_exe:stop(Pid,normal),
   {comment,"Port kill timeout working"}.

port_data(_Config) ->
   ExeSpec=#{ path=>[{app,liberl},"c_src/le_eixx"] },

   ?line {ok,Pid}=gen_exe:start_link(tmod,{ExeSpec,self()},[start,{debug,10}]),
   Count=300,
   T1=now(),
   [ gen_exe:port_cast(Pid,{add,N}) || N<-lists:seq(1,Count) ],
   gen_exe:port_cast(Pid,getsum),

   %Wait for sum to get in
   Sum=receive {data,{sum,Sum1},_ModSate} -> Sum1 end,
   T2=now(),

   %Make sure we received the sum properly calculated
   ct:pal("port_sum info=~p",[Sum]),
   true = Sum==Count*(Count+1)/2,

   Diff=timer:now_diff(T2,T1)/Count,
   gen_exe:port_stop(Pid,"Bye"),
   ?line true=meck:validate(tmod),

   gen_exe:stop(Pid,normal),
   Comment = io_lib:format("ok, ~B msgs sent; ~.3f us/msg~n",[Count,Diff]),
   file:write_file(?STATFILE,Comment,[append]),
   { comment, Comment }.

cast_forward(_Config) ->
   ExeSpec=#{ path=>[{app,liberl},"c_src/le_eixx"] },

   ?line {ok,Pid}=gen_exe:start_link(tmod,{ExeSpec,self()},[start,{debug,10}]),
   gen_server:cast(Pid,cast_req_1),
   cast_req_1=receive {cast,Req} -> Req end,

   gen_exe:stop(Pid,normal),
   { comment, "handle_cast forwarding to module working."}.

info_forward(_Config) ->
   ExeSpec=#{ path=>[{app,liberl},"c_src/le_eixx"] },

   ?line {ok,Pid}=gen_exe:start_link(tmod,{ExeSpec,self()},[start,{debug,10}]),
   Pid ! info_msg_1,
   info_msg_1=receive {info,Msg} -> Msg end,

   gen_exe:stop(Pid,normal),
   { comment, "handle_info forwarding to module working."}.

call_forward(_Config) ->
   ExeSpec=#{ path=>[{app,liberl},"c_src/le_eixx"] },

   ?line {ok,Pid}=gen_exe:start_link(tmod,{ExeSpec,self()},[start,{debug,10}]),
   7=gen_server:call(Pid,{add,3,4}),

   gen_exe:stop(Pid,normal),
   { comment, "handle_call forwarding to module working."}.

exespec(_Config) ->
   ?line ExeSpec=#{name=>"echo",
             path=>[{app,liberl},"c_src/le_eixx"],
             argspec=>[
                        #{id=>str1, default=>" hello, got spaces ",required=>yes},
                        #{id=>str2,               default=>hi,     required=>yes},
                        #{id=>opt1, argopt=>"-o", default=>5,      required=>yes},
                        #{id=>num1, argopt=>"-f", default=>1.0,    required=>yes},
                        #{id=>opt2, argopt=>"-p",                  required=>no}
                      ]},

   {ok,Pid}=gen_exe:start_link(tmod,{ExeSpec,self()},[start,{debug,19}]),
   gen_exe:port_cast(Pid,getcmdline),

   %Test Exespec at start_link
   CmdL=receive {data,L,_} -> L end,
   [_ExecName," hello, got spaces ","hi", "-o 5","-f 1.0"] = CmdL,
   gen_exe:port_stop(Pid,"Bye"),

   %Test Runparams merging with Exespec at start_link
   RunSpec=[{str1,"another string"}],
   {ok,Pid1}=gen_exe:start_link(tmod,{ExeSpec,self()},[start,{debug,19},{runspec,RunSpec}]),
   gen_exe:port_cast(Pid1,getcmdline),
   CmdL1=receive {data,L1,_} -> L1 end,
   [_ExecName,"another string","hi", "-o 5","-f 1.0"] = CmdL1,
   gen_exe:port_stop(Pid1,"Bye"),

   %Test port stop/start keeps runparams
   gen_exe:port_start(Pid1,[]),
   gen_exe:port_cast(Pid1,getcmdline),
   CmdL2=receive {data,L2,_} -> L2 end,
   [_ExecName,"another string","hi","-o 5","-f 1.0"] = CmdL2,
   gen_exe:port_stop(Pid1,"Bye"),

   %Test port_start runspec merging
   gen_exe:port_start(Pid1,[{str1,"I am very happy"},{opt2,pflag}]),
   gen_exe:port_cast(Pid1,getcmdline),
   CmdL3=receive {data,L3,_} -> L3 end,
   [_ExecName,"I am very happy","hi","-o 5","-f 1.0","-p pflag"] = CmdL3,
   gen_exe:port_stop(Pid1,"Bye"),

   gen_exe:stop(Pid1,normal),
   { comment, "Command line argument handling is working."}.

port_call(_Config) ->
   ExeSpec=#{ path=>[{app,liberl},"c_src/le_eixx"] },

   ?line {ok,Pid}=gen_exe:start_link(tmod,{ExeSpec,self()},[start,{debug,10}]),
   %Test call return value
   12.0=gen_exe:port_call(Pid,{multiply,3.0,4.0},5000),

   %Test call timeout
   {OldTT,_} =ct:get_timetrap_info(),
   ct:timetrap(5010),
   ok=try
      %This triggers timeout b/c le_eixx expects doubles,
      %and passing an integer makes it crash
      gen_exe:port_call(Pid,{multiply,3.0,4},5000)
   catch exit:{timeout,_} ->
         ok;
      E:R ->
         E(R)
   end,
   ct:timetrap(OldTT),

   gen_exe:stop(Pid,normal),
   { comment, "port_call working."}.

%%TODO: Tests for module responses,
