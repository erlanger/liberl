-module(le).
-compile(export_all).

%%App configuration api
-export([default/1,default/2,
         opt/1,opt/2,opt/3,
         setopt/2,setopt/3]).
-export([dbg/1]).

-export([dir/1,is_string/1,kvmerge/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("utils.hrl").
%% API


%%===========================================================================
%%App verbosity
%%===========================================================================
dbg(on) ->
   setopt(debug,true);

dbg(off) ->
   setopt(debug,false);

dbg(Level) when is_integer(Level) ->
   setopt(debug,Level).

%%===========================================================================
%%App configuration options
%%===========================================================================
%%TODO: provide option peristance to disk if user desires so

default(Option) ->
   default(app(),Option).

default(App,Option) when is_atom(App) ->
   try
      default_from(Option,App:defaults())
   catch _:_ ->
         undefined
   end.

default_from(Option, Defaults) when is_list(Defaults) ->
   APP=app(),
   case proplists:get_value(Option,Defaults) of
      undefined ->
         error_logger:error_msg("     ~s:Unkown configuration value for '~p'.",[APP,Option]),
         error(badarg);

      Other     -> Other
   end.

setopt(Option,Value) ->
   setopt(app(),Option,Value).

setopt(App, Option, Value) when is_atom(App) ->
   case app() of
      undefined ->
         try
            application:ensure_started(gproc),
            gproc:set_value({p,l,{'$le_opt',Option}},Value),
            ok
         catch error:badarg ->
               gproc:reg({p,l,{'$le_opt',Option}},Value),
               ok
         end;
      App ->
         gproc:set_env(l,App,Option,Value,[app_env]),
         ok
      end.

opt(Option) ->
   opt(app(),Option,default(Option)).

opt(Option,Default) ->
   opt(app(),Option,Default).

opt(App,Option,Default) when is_atom(App) ->
   case app() of
      undefined ->
         try
            application:ensure_started(gproc),
            gproc:get_value({p,l,{'$le_opt',Option}})
         catch error:badarg ->
               Default
         end;
      App ->
         gproc:get_env(l,App,Option,[app_env,{default,Default}])
   end.

appdir() ->
   case app() of
      undefined ->
         error(badarg);
      App ->
         appdir(App)
   end.

appdir(Appmod) when is_atom(Appmod) ->
   %% code:which(..) sometimes returns an atom (e.g. if code coverage is enabled)
   AppmodDir = case file:read_file_info(code:which(Appmod)) of
               {ok,_} -> filename:dirname(code:which(Appmod));
               {error,_} -> {ok,D} = file:get_cwd(),
                            case string:str(D,".eunit") of
                               0 -> error(badarg);
                               _ -> D
                            end
               end,
   filename:dirname(AppmodDir).

%% @doc Joins specified directories, allowing special tags.
%%
%%      The subdirectories can be any of the following or a list of the following:
%%      <dl>
%%      <dt>"Dir"</dt>
%%      <dd>Any directory enclosed in quotes, e.g. "myfiles" or "myfiles/other"</dd>
%%
%%      <dt>`priv'</dt>
%%      <dd>The priv directory for the current application. The application is
%%          presumed to have a module with the name returned by `application:get_application()'.
%%          Use the option below if you have another module name."</dd>
%%
%%      <dt>`{priv,Appmod}'</dt>
%%      <dd>The priv directory for the application which contains the module `Appmod'."</dd>
%%
%%      <dt>`arch'</dt>
%%      <dd>Will substitute the value returned by `erlang:system_info(system_architecture)',
%%          e.g. "x86_64-unknown-linux-gnu"</dd>
%%
%%      <dt>`pwd' or `cwd'</dt>
%%      <dd>Any directory enclosed in quotes, e.g. "myfiles" or "myfiles/other"</dd>
%%
%%      <dt>`home'</dt>
%%      <dd>Substitute the user's home directory e.g. "/home/user"</dd>
%%      </dl>
%%
%%      Example:
%%      ```
%%      1> le:dir([priv,arch]).
%%      "/home/user/mydir/myappp/priv/x86_64-unknown-linux-gnu"
%%      2> le:dir(priv).
%%      "/home/user/mydir/myapp/priv"
%%      3> le:dir([home,"adir","anotherdir"]).
%%      "/home/user/adir/anotherdir"
%%
%%      '''
%%      Note: If several path components are absolute path names,
%%      the last one will take precedence. `home', `priv', `app' and
%%      `cwd' (or `pwd') are absolute path names. `arch' is a relative
%%      path name.
dir(Paths) when is_list(Paths) ->
   case io_lib:char_list(Paths) of
      true -> Paths;
      false ->
         Dirs1=[ case D of
                  priv -> [appdir(),"priv"];
                  {priv,Appmod} -> [appdir(Appmod),"priv"];
                  app  -> appdir();
                  {app,Appmod}  -> appdir(Appmod);
                  arch -> erlang:system_info(system_architecture);
                  cwd  -> {ok,C} = file:get_cwd(),C;
                  pwd  -> {ok,C} = file:get_cwd(),C;
                  home -> {ok, [[H]]} = init:get_argument(home),H;
                  Dir  -> Dir
               end || D <- le:flatdirs(Paths) ],
         filename:join(le:flatdirs(Dirs1))
   end;

dir(Path) when not is_list(Path) ->
   dir([Path]).

%% @doc Merge KeyValue lists, original tuple order is NOT guaranteed.
%%
%%      Kvlist1 takes precedence over Kvlist2 when the keys are
%%      equal. In this case, the tuple from Kvlist2 will be discarded.
%% @end
kvmerge([],Kvlist2) ->
   Kvlist2;

kvmerge(Kvlist1,[]) ->
   Kvlist1;

kvmerge(Kvlist1,Kvlist2) ->
   orddict:merge(fun(_Key,T1,_T2) -> T1 end,
                 orddict:from_list(Kvlist1),
                 orddict:from_list(Kvlist2)).

%Turn ["/etc",arch,["sys","myapp"]] into ["/etc",arch,"sys","myapp"]
flatdirs([]) ->
   [];

flatdirs([H|R]) when is_list(H) ->
   case io_lib:char_list(H) of
      true -> [H|flatdirs(R)];
      false -> flatdirs(H) ++ flatdirs(R)
   end;

flatdirs([H|R]) ->
   [H|flatdirs(R)].


is_string(L) ->
   io_lib:char_list(L).

% Internal functions
% ------------------

app() ->
   application:get_application().

%%%---------------------------------------------------------------------
%%% Unit testing
%%%---------------------------------------------------------------------

-ifdef(EUNIT).
-define(tt(T,F), {T,timeout, 2, ?_test(F)}).
-define(recvMatch(Value,Expr),
        ?assertMatch(Value when Value =/=timeout,
                     begin
                        Expr,
                        receive
                           V -> V
                        after
                           4000 -> timeout
                        end
                     end)
       ).


exec_test_() ->
    {setup,
        fun() ->
            ok
        end,
        fun(ok) ->
            ok
        end,
        [
            ?tt("per process options",test_process_opt()),
            ?tt("application options",test_app_opt()),
            ?tt("dir                ",test_dir()),
            ?tt("kvmerge            ",test_kvmerge()),
            ?tt("miscellaneous      ",test_misc())
        ]
    }.

test_process_opt() ->
   [
      ?assertMatch(true,begin dbg(on), opt(debug) end),
      ?assertMatch(10,begin dbg(10), opt(debug) end),
      ?assertMatch(false,begin dbg(off), opt(debug) end),
      ?assertMatch(15, opt(myopt,15) ),
      ?assertError(badarg,default_from(myopt,[{myopt2,none},{myopt4,none}])),
      ?assertMatch(one,default_from(myopt,[{myopt2,none},{myopt4,none},{myopt,one}]))
   ].

test_app_opt() ->
   [
      ?assertMatch(true,
          begin
             ok=application:start(liberl),
             dbg(on),
             D=application:get_env(liberl,debug),
             application:stop(liberl),
             D
          end),
      ?assertMatch(10,begin dbg(10), opt(debug) end),
      ?assertMatch(false,begin dbg(off), opt(debug) end)
   ].

test_dir() ->
   [
      ?assertMatch("mydir",dir("mydir")),
      ?assertMatch("mydir",dir(mydir)),
      ?assertMatch("dir1/mydir",dir(["dir1","mydir"])),
      ?assertMatch(true,init:get_argument(home)=={ok,[[dir(home)]]}),
      ?assert(begin D=dir({app,le}),string:str(D,"liberl")>0 end),
      ?assert(begin D=dir({priv,le}),string:str(D,"liberl/priv")>0 end),
      ?assertMatch("/",begin file:set_cwd("/"),dir(cwd) end),
      ?assertMatch("/",begin file:set_cwd("/"),dir(pwd) end),
      ?assert(dir([arch])++"\n"==?cmd("gcc -dumpmachine"))
   ].

test_kvmerge() ->
   [
      ?assertMatch([{one,1},{three,3},{two,2}],
                   kvmerge([{one,1},{two,2}],[{three,3},{one,2}])),
      ?assertMatch([{three,3},{one,2}],
                   kvmerge([],[{three,3},{one,2}])),
      ?assertMatch([{three,3},{one,2}],
                   kvmerge([{three,3},{one,2}],[])),
      ?assertMatch([],
                   kvmerge([],[]))
   ].
test_misc() ->
   [
      ?assert(is_string("hello")),
      ?assert(is_string(55)/=true)
   ].
-endif.
