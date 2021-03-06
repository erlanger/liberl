-module(gen_exe_example).
-behaviour(gen_exe).

-export([run/0,terminate/1,init/1,port_start/3,port_exit/2,port_data/4]).

-include_lib("liberl/include/utils.hrl").

run() ->
   gen_exe:start_link(?MODULE,#{ path => "echo 1>&2",
                                 argspec => [
                                             #{id=>count}, %First command-line argument
                                             #{id=>type}   %Second command-line argument
                                            ]
                               },[start,shell] ).

init([]) ->
   {ok,#{number=>1, fruit=>"apples"}}.

%  Info is a map with the following information:
%  #{status             =>    The integer exit value of the application or
%                             undefined if we don't have one. If the program
%                             returned a value greater than 127 it means it
%                             died with a signal.  You can call
%                             gen_exe:status(Status), and it will return either
%                             an integer if the program did not die with a
%                             signal or the tuple {signal, Signal, CoreDumped}
%                             where Signal is an atom indicating the signal
%                             (e.g. sighup) and CoreDumped is a boolean
%                             indicating if a memory dump was requested at the
%                             termination of the program.
%
%    restarts_normal    =>    Count of the number of times the program was
%                             restarted after it exited with a zero status
%                             code. A program is restarted either because of
%                             the keep_alive option to gen_server or because
%                             one of the callbacks returns {restart, ...}
%
%    restarts_abnormal  =>    Same as above, but when the program exited
%                             with a non zero status.
%
%    runparams          =>    A list of {Id,Value tuples}, indicating all the
%                             command-line parameters used in the current
%                             execution of the program.  Each command-line
%                             parameter is identified by an Id which is
%                             specified in the Exespec given to gen_exe}.

% State is the user-defined state that is given to each callback.

% port_start(pre_start,...) can return the following tuples:
% {ok,State}                  * gen_exe keeps the state for the next callback
%                             * port is started with the current runparams
%
% {ok, Runparams ,State}      * gen_exe keeps the state for the next callback
%                             * port is started with a merge of Runparams and
%                               the current runparams. Any value in Runparams
%                               takes precedence over an existing valuein the
%                               current runparams.

%
% {stop, Reason  }            * port is not started
%
%
port_start(pre_start,Info,State=#{number:=Number,fruit:=Fruit}) ->
   Params = [
              { count, integer_to_list(Number) },
              { type , Fruit                   }
            ],
   {ok,Params, State};


% port_start(post_start,...) can return the following tuples:
% {ok,State}                  * gen_exe keeps the state for the next callback
%                             * port is started with the current runparams
%
% {ok, Runparams ,State}      * gen_exe keeps the state for the next callback
%                             * Runparams is merged with the runparams with
%                               whcih the port was started, and the result
%                               will be used to generate the command-line
%                               for the next execution of the port. Any value
%                               in Runparams takes precedence over an existing
%                               valuein the current runparams.
%
% {stop, Reason  }            * port is stopped
%
%
port_start(post_start,Info,State=#{number:=Number}) ->
   Count=integer_to_list(Number+1),
   {ok, [ {count,Count} ],  State#{number:=Number+1} }.

% port_exit(...) can return the following tuples:
% {ok,State}                  * gen_exe keeps the state for the next callback
%                             * port stays closed
%
% {restart, Runparams ,State} * gen_exe keeps the state for the next callback
%                             * Port is restarted, Runparams is merged with the
%                               current runparams and the result will be used
%                               to restart the port. Any value in Runparams
%                               takes precedence over an existing valuein the
%                               current runparams.
%
% {restart, State  }          * gen_exe keeps the state for the next callback
%                             * port is restarted with the current runparams
%
%
port_exit(Info,State=#{number:=N}) when N >= 4 ->
   io:format("My port finished, too many fruits!!: ~p~n~n~n",[Info]),
   {ok,State};

port_exit(Info,State=#{fruit:="apples"}) ->
   {restart,[{type,"oranges"}],State#{fruit:="oranges"}};

port_exit(Info,State=#{fruit:="oranges"}) ->
   {restart,[{type,"apples"}],State#{fruit:="apples"}}.

% port_data(post_start,...) can return the following tuples:
% {ok,State}                  * gen_exe keeps the state for the next callback
%                             * port is started with the current runparams
%
% {ok, Runparams ,State}      * gen_exe keeps the state for the next callback
%                             * Runparams is merged with the current runparams,
%                               and the result will be used for the next
%                               execution of the port (the result will become
%                               the new runparams). Any value in Runparams
%                               takes precedence over an existing value in the
%                               current runparams.
%
% {stop, Reason  }            * port is stopped
port_data(_Type,_Data,_Info,State) ->
   %This port doesn't send data
   {ok, State}.

terminate(_Reason) ->
   ok.
