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
   ok=meck:new(tmod, [non_strict,no_link]),

   ok=meck:expect(tmod,init,fun(Arg) ->
            application:ensure_started(gproc),
            gproc:reg({p,l,state}),
            {ok,tmod:state([{init,[Arg]}])} end),

   ok=meck:expect(tmod,port_start, fun(When,Info,State) ->
            %try gproc:unreg({n,l,port_exit_called}) catch _:_ -> ok end,
            {ok,tmod:state([ {When,[Info]} |State])} end),

   ok=meck:expect(tmod,port_exit,fun (Info,State) ->
            try gproc:reg({n,l,port_exit_called},Info) catch _:_ -> ok end, %this is only for the test SUITE!!
            {ok, tmod:state([ {port_exit,[Info]} |State])} end),

   ok=meck:expect(tmod,terminate,fun (_Reason) ->
            try gproc:unreg({n,l,port_exit_called}) catch _:_ -> ok end, %this is only for the test SUITE!!
            ok end),

   ok=meck:expect(tmod,state,fun (S) ->
            gproc:set_value({p,l,state},S),
            S end),

   ok=meck:expect(tmod,state,fun() ->
            gproc:get_value({p,l,state}) end),
   ct:pal("meck tmod started"),

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
    Config.

callorder() ->
    [{userdata,[{doc,"Testing the gen_exe module"}]}].


callorder(_Config) ->
   % Test is done for both shell mode and non shell mode
   [ begin 
        ExeSpec=#{ path=>["/usr/bin/echo"] },
        ?line {ok,Pid}=gen_exe:start_link(tmod,ExeSpec,Mode ++ [{debug,10}]),
        gproc:await({n,l,port_exit_called}),
        gproc:unreg({n,l,port_exit_called}),
        R = sys:get_state(Pid),

        %Make sure callbacks were executed in order
        ?line [{port_exit,_},{post_start,_},{pre_start,_},{init,_}] = maps:get(state,R),
        ct:pal("~p~ngen_exe state=~p",[Mode,R]),
        ?line true=meck:validate(tmod)
    end
    || Mode <- [[start],[start,shell]] ],
   {comment,"Good, callbacks running in order."}.

keep_alive(_Config) ->
   Count=50,
   CountMinusOne=Count-1,
   ExeSpec=#{ path=>["/usr/bin/echo"] },
   ?line {ok,Pid}=gen_exe:start_link(tmod,ExeSpec,[start,keep_alive,{debug,10}]),
   T1=now(),
   C1=[begin  
       {_Pid,Info}=gproc:await({n,l,port_exit_called}),
       gproc:unreg({n,l,port_exit_called}),
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
