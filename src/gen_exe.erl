
-module(gen_exe).
-behaviour(gen_server).
%-compile(export_all).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(DEF_STOP_TIMEOUT,5000).
-define(LEMSG,'$le_msg').

%% ------------------------------------------------------------------
%% User API
%% ------------------------------------------------------------------
-export([start_link/3,start_link/4]).
-export([port_start/0,port_start/1,port_start/2,
         port_call/1,port_call/2,port_call/3,
         port_cast/1,port_cast/2,
         port_stop/1,port_stop/2,port_stop/3,
         get/1,get/2,
         stop/1,stop/2]).


%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%% ------------------------------------------------------------------
%% Behaviour definition
%% ------------------------------------------------------------------

-callback port_exit( Info   :: map(),
                     State  :: term() ) -> {ok, State}       |
                                           {restart, State}  |
                                           {restart, Runparams :: runspec(), State}.

-callback port_start( When  :: pre_start | post_start,
                      Info  :: map(),
                      State :: term()  ) -> {ok,State}                            |
                                            {ok, Runparams :: runspec(),State}    |
                                            {stop, Reason :: term() }.

-callback port_data(  Type  :: terms  | binary ,
                      Info  :: map(),
                      Data  :: term() | binary(),
                      State :: term()  ) -> {ok,State}                            |
                                            {ok, Runparams :: runspec(),State}    |
                                            {stop, Reason :: term() }.

%% ------------------------------------------------------------------
%% Type specs and includes
%% ------------------------------------------------------------------
-include("utils.hrl").
-type path_element() :: string() | priv | arch | app | {priv,module()} | {app, module()}.
-type argid()        :: string().
-type argname()      :: string().
-type argopt()       :: string().
-type argvalue()     :: iolist() | default.

-type arg_spec() :: #{argid()  => #{ id       => atom(),    %Id is required if argspec present
                                     argopt   => argopt(),  %all are optional
                                     name     => argname(), %default is "Unknown arg"
                                     default  => argvalue(),%default is ""
                                     required => yes|no }}. %default is no

-type exespec()   :: #{path    => path_element() | [ path_element() ],
                       name    => nonempty_string(), %optional
                       argspec => [arg_spec()],      %optional
                       exit_codes => [ #{ integer() => string() } ] %optional
                      }.

-type runspec()   :: [{argid(), argvalue()}].

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
%% @doc start gen_exe with a link to supervisor.
%%      In addition to the @{link gen_server} options, the following
%%      are also available:
%%      <dl>
%%      <dt>start</dt>
%%      <dd>Execute the program immediately as gen_exe starts
%%          with the parameters marqued as required in the
%%          `ExeSpec'.</dd>
%%
%%      <dt>{start, RunParams}</dt>
%%      <dd>Execute the program immediately as gen_exe starts
%%          with the specified parameters.</dd>
%%
%%      <dt>keep_alive</dt>
%%      <dd>Restart process if it dies or exits for any reason,
%%
%%      <dt>shell</dt>
%%      <dd>Run process by invoking the shell (default is not to
%%          use the shell).
%%      </dd>
%%
%%      <dt>debug</dt>
%%      <dd>Set debug level to 1</dd>
%%
%%      <dt>{debug,Level}</dt>
%%      <dd>Set debug level to `Level'</dd>
%%      </dl>
%% @end
start_link(ServerName,Module,ArgsIn,Options) ->
   %TODO: Check Options, Exespec, and print good error messages
   case proplists:get_value(runspec,Options) of
      undefined ->
         gen_server:start_link(ServerName,gen_exe,
                               {Module,ArgsIn,Options},Options);

      RunSpec ->
         gen_server:start_link(ServerName,gen_exe,
                               {Module,ArgsIn,RunSpec,Options},Options)
   end.

start_link(Module,ArgsIn,Options) ->
   case proplists:get_value(runspec,Options) of
      undefined ->
         gen_server:start_link(gen_exe,
                               {Module,ArgsIn,Options},Options);

      RunSpec ->
         gen_server:start_link(gen_exe,
                               {Module,ArgsIn,RunSpec,Options},Options)
   end.


port_start() ->
   port_start([]).

port_start(Runparams) when is_list(Runparams) ->
   port_start(self(),Runparams).

port_start(ServerRef,Runparams) when is_list(Runparams) ->
   gen_server:call(ServerRef,{?LEMSG,runit,Runparams,[]}).

port_cast(Msg) ->
   port_cast(self(),Msg).

port_cast(ServerRef,Msg) ->
   gen_server:cast(ServerRef,{?LEMSG,send_msg,Msg}).

port_call(Msg) ->
   port_call(self(),Msg,5000). %Same timeout as gen_server

port_call(Msg,Timeout) ->
   port_call(self(),Msg,Timeout).

port_call(ServerRef,Msg,Timeout) ->
   gen_server:call(ServerRef,{?LEMSG,port_call,Msg},Timeout).

port_stop(Reason) when not is_pid(Reason) ->
   port_stop(self(),Reason).

port_stop(ServerRef,Reason) ->
   port_stop(ServerRef,Reason,?DEF_STOP_TIMEOUT).

port_stop(ServerRef,Reason,Timeout) ->
   gen_server:call(ServerRef,{?LEMSG,stopit,Reason,Timeout},Timeout+1000).

get(What) when is_atom(What) ->
   get(self(),What).

get(ServerRef,What) ->
   gen_server:call(ServerRef,{?LEMSG,get,What}).

stop(Reason) when not is_pid(Reason) ->
   stop(self(),Reason).

stop(ServerRef, Reason) ->
   gen_server:cast(ServerRef,{?LEMSG,stop,Reason}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({Module,{ExeSpec,Args},Options}) ->
   init({Module,{ExeSpec,Args},[],Options});

init({Module,{ExeSpec,Args},RunSpec,Options}) ->
   process_flag(trap_exit, true), %make sure terminate is called if necessary
   {module,_} = code:ensure_loaded(Module),
   le:setopt(debug,proplists:get_value(debug,Options,false)),
   State=#{module     =>Module,         %User module
           exespec    =>ExeSpec,        %Executable description
           runspec    =>RunSpec,        %Runnning parameters [{key,value}]
           port       =>undefined,      %Erlang port connecting to executable
           exit_status=>undefined,      %Last exit status of executable
           opts       =>Options,        %Options given by user on start
           state      =>undefined,      %User module state
           dmode      =>term,           %Default is to use binary_to_term
                                        %can be rawbinary or term
           running    =>undefined,      %Closing state of port, see stopit message
           stopped_notify => undefined, %Whom to notify after calling port_stop(...)
           endreason  =>undefined       %User reason for finishing
          },
   UserResp = call(Module,init,Args),
   application:ensure_started(gproc),
   gproc:add_local_counter({restarts,normal}, 0),
   gproc:add_local_counter({restarts,abnormal}, 0),
   case proplists:get_value(start,Options) of
      A when A==false orelse A==undefined ->
         get_response(init,UserResp,State);
      true  ->
         init(start, UserResp, State);

      RS1 when is_list(RS1)  ->
         RunSpec1=le:kvmerge(RS1,RunSpec),
         init(start, UserResp, State#{runspec:=RunSpec1})
   end;

init({Module,ExeSpec,RunSpec,Options}) ->
   init({Module,{ExeSpec,[]},RunSpec,Options});

init({Module,ExeSpec,Options}) ->
   init({Module,{ExeSpec,[]},[],Options}).

init(start,{stop,Reason},_State) ->
   {stop,Reason};

init(start,ignore,_State) ->
   ignore;

init(start,UserResp,State) ->
   State1=start_port(State#{state:=element(2,UserResp)}),
   % state in the State1 map is the user state which
   % could have been updated by the return value of
   % port_start(...) in the user's module
   UserResp1=setelement(2,UserResp,maps:get(state,State1)),
   get_response(init,UserResp1,State1).


% CALL: Get
% ------------------
handle_call({?LEMSG,get,What}, _From, State) ->
   { reply, maps:get(What,State,undefined), State };


% CALL: Run executable
% --------------------
% TODO: Opts
handle_call({?LEMSG,runit,_RunSpec,Opts}, _From,
            State=#{port:=Port,exespec:=ES}) when is_port(Port) ->
   {reply, {error, {already_started,pathname(ES)}}, State};

handle_call({?LEMSG,runit,RunSpec1,_Opts}, _From,
            State=#{port:=undefined,runspec:=RunSpec2}) ->
   RS3=le:kvmerge(RunSpec1,RunSpec2),
   State1=start_port(State#{runspec:=RS3}),
   {reply, ok, State1};

% CALL: Stop executable
% --------------------
%Nothing to stop if port is not running
handle_call({?LEMSG,stopit,_Reason,_Timeout}, _From, State=#{port:=undefined}) ->
   {reply, {error,port_not_started}, State};

handle_call({?LEMSG,stopit,_Reason,_Timeout}, _From, State=#{running:=stopping}) ->
   {reply, {ok, in_progress}, State};

%Stop the port and if it doesn't exit within Timeout, kill it.
handle_call({?LEMSG,stopit,Reason,Timeout}, From, State=#{port:=Port,running:=yes})
      when is_port(Port) ->
   initiate_port_stop(Port,Timeout,Reason),
   {noreply, State#{running:=stopping,stopped_notify:=From,endreason:=Reason}};

%Ignore other stopit messages (e.g. when port is closing)
handle_call({?LEMSG,stopit,_Reason,_Timeout}, _From, State) ->
   {noreply, State};

% CALL: Make call to port
% --------------------------
handle_call({?LEMSG,port_call,_Req}, _From, State=#{port:=undefined}) ->
   %% Drop messages sent to port that has exited or doesn't exist
   {reply, {error,port_not_started}, State};

handle_call({?LEMSG,port_call, Req}, From,
            State=#{port:=Port,dmode:=Dmode,exespec:=ES}) when is_port(Port) ->
   case Dmode of
      term ->
         Tag=make_ref(),
         port_command(Port,term_to_binary({le_call,Req,Tag})),
         gproc:reg({p,l,{'$le_port_call',Tag}},From);

      rawbinary ->
         error_logger:error_msg("     port_call is not supported in rawbinary mode: ~p.~n",
                          [pathname(ES)]),
         error(badarg)
   end,
   {noreply, State};

% CALL: Unknown call - forward to module
% --------------------------------------
handle_call(Request, From, State=#{module:=M,state:=UState}) ->
   say(7,"   ~s: Resending call to module ~p",[M,Request]),
   try
      UserResp=M:handle_call(Request,From,UState),
      get_response(handle_call,UserResp,State)
   catch error:undef ->
         {reply, badcall, State}
   end.


% INFO: Port messages (gen_exe port sends these)
% ----------------------------------------------
handle_info({Port, {data, Bin}}, State = #{module:=M,port:=Port,exespec:=ES, dmode:=Dmode} ) ->
   State2=case Dmode of
      term ->
         Term=binary_to_term(Bin),
         say(8,"   ~s: Received from port ~s: ~200p",[M,esname(ES),Term]),
         case Term of
            %This is the reply to a port_call, send reply back to caller
            {le_reply,Reply,Tag} ->
               From=gproc:get_value({p,l,{'$le_port_call',Tag}}),
               gen_server:reply(From,Reply),
               gproc:unreg({p,l,{'$le_port_call',Tag}}),
               State;

            %This is simply a msg received from the port, forward to port_data
            _Other ->
               call(port_data,Term,State)
         end;

      rawbinary ->
         say(8,"   ~s: Received from port ~s: ~200p",[M,esname(ES),Bin]),
         call(port_data,Bin,State)
   end,
   {noreply, State2};


% INFO: Port finished
% -------------------
handle_info({Port, {exit_status, Status}}, State=#{port:=Port}) ->
   {noreply,State#{exit_status:=Status}};

%The two equal Port's in the pattern match assure that the right Port is being handled
handle_info({'EXIT', Port, Reason}, State=#{port:=Port,running:=stopping,stopped_notify:=From}) ->
   gen_server:reply(From,{ok,Reason}),
   dsay("Replied to ~p {ok,~p}",[From,Reason]),
   State1=handle_port_exit(State,Reason),
   {noreply,State1#{stopped_notify:=undefined}};

handle_info({'EXIT', Port, Reason}, State=#{port:=Port}) ->
   State1=handle_port_exit(State,Reason),
   {noreply,State1};

% INFO: Unknown info msg - forward to module
% ------------------------------------------
handle_info(Msg, State=#{module:=M,state:=UState}) ->
   say(7,"   ~s: Resending msg to module ~p",[M,Msg]),
   try
      UserResp=M:handle_info(Msg,UState),
      get_response(handle_info,UserResp,State)
   catch error:undef ->
         {noreply, State}
   end.

% CAST: Stop gen_exe
% --------------------
handle_cast({?LEMSG,stop,Reason}, State) ->
   {ok,_}=timer:apply_after(1,gen_exe,port_stop,[Reason]),
   {ok,_}=timer:apply_after(?DEF_STOP_TIMEOUT + 10,gen_server,cast,[self(),{?LEMSG,terminate,Reason}]),
   {noreply, State};

handle_cast({?LEMSG,terminate,Reason}, State) ->
   {stop, Reason, State};

% CAST: Run executable
% --------------------
handle_cast({?LEMSG,runit,restart,Counter}, State) ->
   State1=start_port(State),
   gproc:update_counter({c,l,{restarts,Counter}},1),
   {noreply, State1};


% CAST: Stop executable
% ---------------------

%Kill the port because it exit within the timeout period specified in the stopit message
handle_cast({?LEMSG,killit,Port,Reason}, State=#{port:=Port,exespec:=ES,running:=stopping}) ->
   say(2,"   Killing port ~p~n"
       "      Reason: ~p",[esname(ES),Reason]),
   port_close(Port), %This should trigger an EXIT msg which will set running:=no
   {noreply, State};

%Ignore other killit messages (e.g. if port has closed)
handle_cast({?LEMSG,killit,_Port,_Reason}, State) ->
   {noreply, State};

% CAST: send message to port
% --------------------------
handle_cast({?LEMSG,send_msg,_Msg}, State=#{port:=undefined}) ->
   %% Drop messages sent to port that has exited or doesn't exist
   {noreply, State};

handle_cast({?LEMSG,send_msg,Msg}, State=#{port:=Port,dmode:=Dmode}) when is_port(Port) ->
   case Dmode of
      term ->
         port_command(Port,term_to_binary(Msg));
      rawbinary ->
         port_command(Port,Msg)
   end,
   {noreply, State};

% CAST: Unknown cast - forward to module
% --------------------------------------
handle_cast(Msg, State=#{module:=M,state:=UState}) ->
   say(7,"   Resending cast to module ~p: ~p",[M,Msg]),
   try
      UserResp=M:handle_cast(Msg,UState),
      get_response(handle_cast,UserResp,State)
   catch error:undef ->
         {noreply, State}
   end.

% Terminate & Code change
% -----------------------
terminate(Reason, #{module:=M,state:=UState}) ->
   port_cast({stop,Reason}),
   say(2,"   Terminating due to ~p",[Reason]),
   try
   say(7,"   Calling ~p:terminate(~p,~p)",[M,Reason,UState]),
      M:terminate(Reason,UState)
   catch error:undef ->
         ok
   end.

code_change(OldVsn, State=#{module:=M,state:=UState}, Extra) ->
   try
      say(7,"   Calling ~p:code_change(~p,~p,~p)",
          [M,OldVsn,UState,Extra]),
      M:code_change(OldVsn,UState,Extra)
   catch error:undef ->
         {ok, State}
   end.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
get_info(#{exespec:=ES,runspec:=RS,
           exit_status:=Status}) ->
   RP=get_runparams(ES,RS),
   #{status             => Status,
     restarts_normal    => gproc:get_value({c,l,{restarts,normal}}),
     restarts_abnormal  => gproc:get_value({c,l,{restarts,abnormal}}),
     runparams          => RP}.

handle_port_exit(State=#{exespec:=ES,runspec:=RS,opts:=Opts,
                         module:=M,exit_status:=Status},Reason) ->
   Counter=case Status of
      0 -> normal;
      _ -> abnormal
   end,
   say(2,"   ~s: ~p port terminated with status ~p~n"
         "   Reason: ~p~n",
         [M,esname(ES),status_msg(Status,ES),Reason]),
   State1 = State#{port:=undefined},

   Restart=call(port_exit,State),


   State2=case proplists:get_bool(keep_alive,Opts) of
      true ->
         restart_port(State1#{state:=element(tuple_size(Restart),Restart)},
                      Counter);

      false -> case Restart of
            {restart, RS1, UState} ->
               RS2=le:kvmerge(RS1,RS),
               restart_port(State1#{runspec:=RS2,state:=UState},Counter);

            {restart, UState}      ->
               restart_port(State1#{state:=UState},Counter);

            {ok, UState}           -> State1#{state:=UState,running:=no};

            Other                  -> bad_response(M,port_exit,Other)
         end
   end,
   State2.

restart_port(State,Counter) when is_atom(Counter)->
   gen_server:cast(self(),{?LEMSG,runit,restart,Counter}),
   State.


start_port(State=#{port:=Port,exespec:=ES,running:=yes}) when is_port(Port) ->
   error_logger:error_msg("     Port ~p is already started.~n",
                          [pathname(ES)]),
   State;

start_port(State=#{port:=undefined,module:=M,runspec:=RS}) ->
   %Run executable
   case  call(port_start,pre_start,State) of
      {stop, Reason} -> State#{endreason:=Reason,running:=no};

      {ok,RunSpec,UState}       ->
         % User RunSpec takes priority in the merge with State's RS
         RS2=le:kvmerge(RunSpec,RS),
         start_port1(State#{state:=UState},RS2);

      {ok, UState} ->
         start_port1(State#{state:=UState},RS);

      Other                  -> bad_response(M,port_start,Other)
   end.

start_port1(State=#{module:=M,exespec:=ExeSpec,opts:=Opts},NewRS) ->
   Dir=filename:dirname(pathname(ExeSpec)),
   PortOpts = [ {cd, Dir},
               %{args,["2"]},
               binary,
               exit_status,
               %stream,
               {packet,4},
               use_stdio, hide],
   {Spawn,Exe, PortOpts1}=case proplists:get_bool(shell,Opts) of
      true  -> {spawn,
                get_exe(ExeSpec,NewRS),
                PortOpts};
      false -> {spawn_executable,
                pathname(ExeSpec),
                PortOpts ++ [{args, args2strl(ExeSpec,NewRS) }]}
   end,
   say(1, "   ~s: Starting port ~p", [M,Exe]),
   say(2, "          port options: ~p", [PortOpts1]),
   try
      Port=erlang:open_port({Spawn, Exe}, PortOpts1),
      State1=State#{port:=Port,runspec:=NewRS,exit_status:=undefined},
      % post_start call merges the user RunSpec with the state RunSpec
      % and updates the user UState
      State2=case call(port_start,post_start,State1) of
         {stop,Reason} -> %This will trigger a port_exit which will set running:=no
                          initiate_port_stop(Port,?DEF_STOP_TIMEOUT,Reason),
                          State1#{endreason:=Reason};

         NewState      -> NewState#{running:=yes}
      end,
      State2
   catch _:Reason1 ->
         error_logger:error_msg("     ~s: Unable to start port ~p~n"
                                "     Reason: ~p~n",
                                [M,pathname(ExeSpec),Reason1]),
         exit(port_not_started)
   end.

initiate_port_stop(Port,Timeout,Reason) when is_port(Port) ->
   %Ask port to close, give it time to finish
   port_cast({stop,Reason}),
   try
      {ok,_}=timer:apply_after(Timeout,gen_server,cast,[self(),{?LEMSG,killit,Port,Reason}])
   catch error:badmatch ->
         %% Couldn't setup timer, send msg to kill it immediately
         gen_server:cast(self(),{?LEMSG,killit,Port,Reason})
   end.


% Module calls
% ============

-spec bad_response(Module::atom(),Function::atom(),Response::term()) -> no_return().
bad_response(M,F,Resp) ->
   error_logger:error_msg("     ~s: Bad response from ~p:~p~n"
                          "     ~p~n",
                          [?MODULE,M,F,Resp]),
   error(bad_response),
   bad.

check_fun(M,F,A,NotExportedValue) ->
   case erlang:function_exported(M,F,A) of
      true  -> error(undef); %This was thrown from the user's module originally
      false ->
         say(4,"   Warning: Function ~p:~p/~B not exported",[M,F,A]),
         NotExportedValue
   end.


call(port_start,pre_start,State=#{state:=UState,module:=M}) ->
   try
      Info=get_info(State),
      say(7,"   Calling ~p:port_start(pre_start,~p,~p)",[M,Info,UState]),
      M:port_start(pre_start,Info,UState)
   catch error:undef ->
      check_fun(M,port_start,3,{ok, UState})
   end;

call(port_start,post_start,S=#{state:=UState,module:=M,
                               runspec:=RS}) ->
   try
      Info=get_info(S),
      say(7,"   Calling ~p:port_start(post_start,~p,~p)",[M,Info,UState]),
      case M:port_start(post_start,Info,UState) of
         {ok,UState1}     -> S#{state:=UState1};
         {ok,RS1,UState1} -> S#{runspec:=le:kvmerge(RS1,RS),state:=UState1};
         {stop, Reason}   -> {stop, Reason};
         Other            -> bad_response(M,port_start,Other)
      end
   catch error:undef ->
         check_fun(M,port_start,3,S)
   end;

call(port_data,Data,S=#{dmode:=Dmode,module:=M,state:=UState,runspec:=RS,port:=Port}) ->
   try
      Info=get_info(S),
      say(7,"   Calling ~p:port_data(~p,~p,~p,~p)",[M,Dmode,Data,Info,UState]),
      case M:port_data(Dmode,Data,Info,UState) of
         {ok,UState1}     -> S#{state:=UState1};
         {ok,RS1,UState1} -> S#{runspec:=le:kvmerge(RS1,RS),state:=UState1};
         {stop, Reason}   -> %This will trigger port_exit which will set running:=no
                             initiate_port_stop(Port,?DEF_STOP_TIMEOUT,Reason),
                             S#{endreason:=Reason};
         Other            -> bad_response(M,port_data,Other)
      end
   catch error:undef ->
         check_fun(M,port_data,4,S)
   end;

call(M,init,Args) ->
   try
      say(7,"   Calling ~p:init(~p)",[M,Args]),
      M:init(Args)
   catch error:undef ->
         check_fun(M,init,1,{ok, undefined})
   end.

call(port_exit,State=#{state:=UState,module:=M}) ->
   try
      Info=get_info(State),
      say(7,"   Calling ~p:port_exit(~p,~p)",[M,Info,UState]),
      M:port_exit(Info,UState)
   catch error:undef ->
         check_fun(M,port_exit,2,{ok,undefined})
   end.

get_response(init,UserResp,State) ->
   case UserResp of
      {ok, UState} ->
         {ok, State#{state:=UState}};
      {ok, UState, Timeout } ->
         {ok, State#{state:=UState}, Timeout };
      {stop, Reason} ->
         {stop, Reason};
      ignore -> ignore
   end;

get_response(handle_info,UserResp,State) ->
   get_response(handle_cast,UserResp,State);

get_response(handle_cast,UserResp,State) ->
   case UserResp of
      {noreply, UState} ->
         {noreply, State#{state:=UState} };
      {noreply, UState, Timeout } ->
         {noreply, State#{state:=UState}, Timeout };
      {stop, Reason, UState} ->
         {stop, Reason, State#{state:=UState}}
   end;

get_response(handle_call,UserResp,State) ->
   case UserResp of
      {reply, Reply, UState} ->
         {reply, Reply, State#{state:=UState} };
      {reply, Reply, UState, Timeout} ->
         {reply, Reply, State#{state:=UState}, Timeout};
      {noreply, UState } ->
         {noreply, State#{state:=UState} };
      {noreply, UState, Timeout} ->
         {noreply, State#{state:=UState}, Timeout};
      {stop, Reason, Reply, UState} ->
         {stop, Reason, Reply, State#{state:=UState} };
      {stop, Reason, UState} ->
         {stop, Reason, State#{state:=UState} }
   end.

% Human message for program exit status code
status_msg(Status,ExeSpec) ->
      Codes=maps:get(exit_codes,ExeSpec,#{ 0 => "OK"}),
      maps:get(Status,Codes,?FMT("~p - Unknown exit value ",[status(Status)])).

% ExeSpec,RunSpec and Argument handling
% -------------------------------------
pathname(ExeSpec) when is_map(ExeSpec) ->
   le:dir(maps:get(path,ExeSpec)).

argspec(Id,ExeSpec) ->
   ArgsSpec=maps:get(argspec,ExeSpec,[]),
   L=[ Arg || Arg=#{id:=Id1} <- ArgsSpec,Id1==Id ],
   case L of
      [] -> [];
      _  -> hd(L)
   end.

argopt(Id,ExeSpec) ->
   maps:get(argopt,argspec(Id,ExeSpec),"").

%argname(Id,ExeSpec) ->
%   maps:get(name,argspec(Id,ExeSpec),"Unknown arg").

argvalue(Id,Value,ExeSpec) ->
   argvalue(Id,Value,ExeSpec,no).

argvalue(Id,Value,ExeSpec,QuoteSpaces) ->
   Val=to_str(case Value of
      default -> maps:get(default,argspec(Id,ExeSpec),"");
      Any     -> Any
   end),
   case QuoteSpaces of
      yes -> quote_spaces(Val);
      _   -> Val
   end.

%lists of {Key,Value} pairs currently active
get_runparams(ExeSpec,RunSpec) when is_map(ExeSpec) andalso is_list(RunSpec) ->
   ArgsSpec=maps:get(argspec,ExeSpec,[]),
   lists:foldl(
         fun(#{id:=Id}=AS,Acc) ->
               case lists:keysearch(Id,1,RunSpec) of
                  {value,{Id,Value}} -> %It's in Runspec, put it in
                     Acc ++ [{Id,argvalue(Id,Value,ExeSpec)}];

                  false      -> %Put it in if it is required by ExeSpec
                     case AS of
                        #{required:=yes} ->
                           Acc ++ [{Id,argvalue(Id,default,ExeSpec)}];

                        _NotRequired     -> Acc
                     end
               end
         end,
         [],
         ArgsSpec).

args2strl(ExeSpec,RunSpec) ->
   args2strl(ExeSpec,RunSpec,no).

args2strl(ExeSpec,RunSpec,QuoteSpaces) when is_map(ExeSpec) andalso is_list(RunSpec) ->
   ArgsSpec=maps:get(argspec,ExeSpec,[]),
   lists:foldl(
      fun(#{id:=Id}=AS,Acc) ->
            case lists:keysearch(Id,1,RunSpec) of
               {value,{Id,Value}} -> %It's in Runspec, put it in
                  lists:append([Acc,[ optandvalue(Id,ExeSpec,Value,QuoteSpaces) ]]);

               false      -> %Put it in if it is required by ExeSpec
                  case AS of
                     #{required:=yes} ->
                        lists:append([Acc,[ optandvalue(Id,ExeSpec,default,QuoteSpaces) ]]);

                     _NotRequired     -> Acc
                  end
            end
      end,
      "",
      ArgsSpec).

optandvalue(Id,ExeSpec,Value,QuoteSpaces) ->
   Opt=argopt(Id,ExeSpec),
   Value1=argvalue(Id,Value,ExeSpec,QuoteSpaces),
   case Opt=="" orelse Value1=="" of
      true  -> ?FMT("~s~s",  [Opt,Value1 ]);
      false -> ?FMT("~s ~s", [Opt,Value1 ])
   end.

esname(ExeSpec) when is_map(ExeSpec) ->
   maps:get(name,ExeSpec,"unnamed").

quote_spaces(Str) ->
   case io_lib:char_list(Str) of
      true ->
   lists:flatmap(fun (C) when C==32 -> [$\\,C];
                     (C) -> [C]
                 end,
                 Str);
      false -> Str
   end.

-spec get_exe(exespec(), runspec()) -> string().
get_exe(ExeSpec,RunSpec) when is_map(ExeSpec)  andalso is_list(RunSpec) ->
   case args2strl(ExeSpec,RunSpec,yes) of
      [""] -> pathname(ExeSpec);
      L    -> pathname(ExeSpec) ++ " " ++  string:join(L," ")
   end.

to_str(Value) when is_integer(Value) ->
   integer_to_list(Value);

to_str(Value) when is_float(Value) ->
   float_to_list(Value,[{decimals,20},compact]);

to_str(Value) when is_atom(Value) ->
   atom_to_list(Value);

to_str(Value) when is_list(Value) ->
   Value. %we'll let it error if it is not a string

%La ciencia del Amor de Dios y de la Oracion
%en la Cuarta Morada de Sta. Teresa de Jesus:
%No consiste en el mayor gusto, sino en:
%1. La mayor determinacion
%2. De desear contentar en todo a Dios
%3. y procurar, en cuanto pudiearamos, nole ofender
%4. y rogarle vaya siempe adelante la gloria de su hijo
%5. y el aumento de la Iglesia Catolica

%% This signal conversion code came from github.com/saleyn/erlexec
%%-------------------------------------------------------------------------
%% @doc Decode the program's exit_status.  If the program exited by signal
%%      the function returns `{signal, Signal, Core}' where the `Signal'
%%      is the signal number or atom, and `Core' indicates if the core file
%%      was generated.
%% @end
%%-------------------------------------------------------------------------
-spec status(integer()) ->
        {status, ExitStatus :: integer()} |
        {signal, Singnal :: integer() | atom(), Core :: boolean()}.
status(Status) when is_integer(Status) ->
    TermSignal = Status band 16#7F,
    IfSignaled = ((TermSignal + 1) bsr 1) > 0,
    ExitStatus = (Status band 16#FF00) bsr 8,
    case IfSignaled of
    true ->
        CoreDump = (Status band 16#80) =:= 16#80,
        {signal, signal(TermSignal), CoreDump};
    false ->
        ExitStatus
    end;

status(undefined) -> undefined.

%%-------------------------------------------------------------------------
%% @doc Convert a signal number to atom
%% @end
%%-------------------------------------------------------------------------
-spec signal(integer()) -> atom() | integer().
signal( 1) -> sighup;
signal( 2) -> sigint;
signal( 3) -> sigquit;
signal( 4) -> sigill;
signal( 5) -> sigtrap;
signal( 6) -> sigabrt;
signal( 7) -> sigbus;
signal( 8) -> sigfpe;
signal( 9) -> sigkill;
signal(11) -> sigsegv;
signal(13) -> sigpipe;
signal(14) -> sigalrm;
signal(15) -> sigterm;
signal(16) -> sigstkflt;
signal(17) -> sigchld;
signal(18) -> sigcont;
signal(19) -> sigstop;
signal(20) -> sigtstp;
signal(21) -> sigttin;
signal(22) -> sigttou;
signal(23) -> sigurg;
signal(24) -> sigxcpu;
signal(25) -> sigxfsz;
signal(26) -> sigvtalrm;
signal(27) -> sigprof;
signal(28) -> sigwinch;
signal(29) -> sigio;
signal(30) -> sigpwr;
signal(31) -> sigsys;
signal(34) -> sigrtmin;
signal(64) -> sigrtmax;
signal(Num) when is_integer(Num) -> Num.

