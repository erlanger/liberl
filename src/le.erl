-module(le).
%-compile(export_all).

%%App configuration api
-export([opt/1,opt/2,opt/3,
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

default_from(Option, Defaults) when is_list(Defaults) ->
   case proplists:get_value(Option,Defaults) of
      undefined ->
         APP=app(),
         error_logger:error_msg("     ~s:Unkown configuration value for '~p'.",[APP,Option]),
         error(badarg);

      Other     -> Other
   end.

setopt(Option,Value) ->
   setopt(app(),Option,Value).

setopt(App, Option, Value) when is_atom(App) ->
   case App of
      no_app ->
         put(Option,Value);
      App ->
         gproc:set_env(l,App,Option,Value,[app_env]),
         ok
      end.

opt(Option) ->
   opt(app(),Option,undefined).

opt(Option,Default) when not is_function(Default) ->
   opt(app(),Option,Default);

opt(Option,Defaultsfun) when is_function(Defaultsfun,0) ->
   opt(app(),Option,default_from(Option,Defaultsfun()));

opt(Option,Defaultfun) when is_function(Defaultfun,1) ->
   opt(app(),Option,Defaultfun(Option)).

opt(App,Option,Default) when is_atom(App) ->
   case App of
      no_app ->
         case get(Option) of
            undefined -> Default;
            Value     -> Value
         end;
      App ->
         try
            gproc:get_env(l,App,Option,[app_env,{default,Default}])
         catch
            %Get around bug in gproc that doesn't return Default if
            %value is not cached
            error:badarg -> Default
         end
   end.

appdir() ->
   case app() of
      no_app ->
         error(badarg);
      App ->
         appdir(App)
   end.

% path needs to be set properly for this to work:
% i.e. -pa ../myapp/ebin and NOT -pa ebin
% see http://erlang.org/pipermail/erlang-questions/2011-October/062024.html
appdir(App) when is_atom(App) ->
   case code:lib_dir(App) of
      Dir when is_list(Dir) -> Dir;

      %% code:which(..) sometimes returns an atom (e.g. if code coverage is enabled)
      {error,_} -> case file:read_file_info(code:which(App)) of
            {ok,_} -> filename:dirname(filename:dirname(code:which(App)));
            {error,_} -> {ok,D} = file:get_cwd(),
               case string:str(D,".eunit") of
                  0 -> error_logger:error_msg("Unable to guess directory for application ~p, ~n"
                                              "please set your code path properly,~n"
                                              " including the application direcotry i.e:~n"
                                              " -pa ../~p/ebin and NOT -pa ebin~n",[App,App]),
                       error(badarg);

                  _ -> filename:dirname(D)
               end
         end
   end.

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
               end || D <- flatdirs(Paths) ],
         filename:join(flatdirs(Dirs1))
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
   case get('$le_appname') of
      undefined ->
         case application:get_application() of
            undefined -> put('$le_appname', no_app),  no_app;
            {ok, App} -> put('$le_appname', App), App
         end;

      App   -> App
   end.

%%%---------------------------------------------------------------------
%%% Unit testing
%%%---------------------------------------------------------------------

-ifdef(EUNIT).
-define(tt(T,F), {T,timeout, 2, ?_test(F)}).

exec_test_() ->
    {setup,
        fun() ->
            ok=application:start(gproc),
            ok=application:start(liberl)
        end,
        fun(ok) ->
            application:stop(gproc),
            application:stop(liberl)
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
      ?assertMatch(one,default_from(myopt,[{myopt2,none},{myopt4,none},{myopt,one}])),
       ?assertMatch(defval2,
             opt(myapp_opt3,
                   fun() ->
                      [ {opt1,def1}, {myapp_opt3,defval2} ]
                   end )
       ),
       ?assertMatch(myvalue4,
             opt(myapp_opt4,
                   fun(myapp_opt4) ->
                      myvalue4
                   end )
       )
   ].

test_app_opt() ->
   [
      ?assertMatch({ok,on},
          begin
             setopt(liberl,myapp_opt,on),
             D=application:get_env(liberl,myapp_opt),
             D
          end),
       ?assertMatch(off,
          begin
             setopt(liberl,myapp_opt1,off),
             D=opt(liberl,myapp_opt1,default),
             D
          end)
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
      ?assert(?debugVal(dir([arch]))==?debugVal(erlang:system_info(system_architecture))),
      ?assert( begin D=dir({app,liberl}), string:str(D,"liberl")>0 end),
      ?assert( begin D=dir({app,code}), string:str(D,"kernel-")>0 end),
      ?assertError(badarg, D=dir({app,crazy_app})),
      ?assertError(badarg, dir(app)), %% This throws an error b/c eunit does not have a default app
      ?assertError(badarg, dir(priv)),%% This throws an error for the same reason
      ?assert(
         begin
            erase(),
            ok=meck:new(application, [unstick,passthrough]),
            ok=meck:expect(application,
                           get_application,
                           fun() -> {ok,kernel} end),
            D=appdir(),
            true=meck:validate(application),
            ok=meck:unload(application),
            string:str(D,"kernel-")>0
         end)
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
