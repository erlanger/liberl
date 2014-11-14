# liberl - Erlang application utilities and gen\_exe for port management

## Dependencies
The library depends on gproc(https://github.com/uwiger/gproc), which is
most useful for almost any erlang application. Also you may want to
include eixx (see below)

## Purpose
I wrote this library because I found myself writing boiler-plate code
for every application I wrote. Code to manage application options,
set debug flags, etc.

The second reason is because I have several applications whose job
is to interact with a small external C++/C executable, and I found
myself writing boiler-plate code all over again, so I decided to make
a generic port server, with a few callbacks. This is the gen\_exe
gen\_server module.

## gen_exe the port management on the erlang side
gen_exe provides the boiler_plate code that we write over and over again every
time that we need to connect to a port program. It also takes cares of tricky
situations, such as when a port doesn't close stdin when it's closed and it
provides the ability to communicate to the port program more or less like a
gen_server. It even supports calls to the port so you can do something like

```12.0=gen_exe:port_call(Pid,{multiply,3.0,4.0})```

The idea is to have a simple call back module that behaves in a
very similar way to a gen_server call back module. In addition to the normal
gen_server handle_call, handle_cast and handle_info events, the module receives
the following port_specific events:

* `port_exit` - when the port program has finished for ANY reason
* `port_data` - when the port program has received data
* `port_start(pre_start...)` - before the port is started
* `port_start(post_start...)` - right after the port has started

The user receives information about the exit status of the port, the number
of times it has been restarted automatically (if requested by the user), etc.
It also provides functions to simplify port communication and execution:
* `port_start` - to start the port whenever the user desires
* `port_stop`  - to stop the port (including timeout based killing if the port
                 does not gently end after receiving a {stop,Reason} message.
* `port_cast`  - to send a message to the port
* `port_call`  - to make a timeout-protected request to the port (just like
                 gen_server call)

It manages port program command-line arguments in parameter-like manner,
automatically merging required default values, and existing run-time parameters
to make it very easy to pass changing command-line arguments to the executable.

gen_exe is just like a gen_server but made for managing ports, in fact it is
a gen_server itself, forwarding any gen_server callbacks to the user module
(i.e. handle_call, handle_info, handle_cast, code_change, terminate, init), in
addition to processing port specific callbacks.

## C++ Port programs - the easy way

liberl, will make it much easier to write the port program in C++ if
you use the eixx library from https://github.com/saleyn/eixx. I
submitted a patch to use it's great marshalling
and variable binding capabilities (in C++!) easily by simply including
a header. The patch is already in the latest master branch.
All you have do (after you have added eixx to your project)
is to include the le\_eixx.hpp. Here is a simple example of a port
program in C++11:
```

#include "le/le_eixx.hpp"

//This program is meant to be used to test the gen_exe server (erlang side)
//working with the le_eixx library (C++ side)

int add(std::vector<int> values)
{ int sum=0; for(auto i: values) sum+=i; return sum; }

//Remember NEVER to use std::cout!!!
//Erlang is using it for communications
void cast1(const char* Msg)
{ std::cerr << "Erlang said " << Msg << std::endl; }

int main(int argc, char *argv[])
{
   using namespace eixx;
   using namespace le;

   std::vector<int> values;

   //You make a dispatcher object first, which tells the le_eixx library
   //how to handle incoming requests
   //
   //Making a dispatcher is very simple:
   //1. Think of the term patterns you want to use
   //2. Provide a function to handle the matches for each pattern
   //
   //When le receives messages from erlang, it finds the first
   //pattern that matches and calls the function you provided.
   //It takes the term returned by your function and sends it back
   //to erlang.
   //
   //The following example performs the following functions:
   //1.  adds numbers given in different erlang messages: {add,Num} and getsum
   //2.  prints an arbitrary String sent from erlang: {print,Msg}
   //3.  stops the event matching loop when erlang asks with a {stop,Reason} msg
   //4.  Prints a warning message if an unknown msg is received and does not send
   //    anything back to erlang.
   //5.  Responds to a gen_exe:port_call({multiply,X,Y})
   //
   //
   auto dp = le::make_dispatcher(
         "{add, Num}",  //Pattern (& because we want to capture values by reference)
               [&] (varbind& vb) { values.push_back(vb["Num"]->to_long());
                                  return le::fmt("{ok,~i}",values.size());}, //Function
         "getsum",
               [&] (varbind& vb) { int res = add(values);
                                  return le::fmt("{sum,~i}",res);},

         "{print, Msg}",
               [] (varbind& vb) {cast1(vb["Msg"]->to_str().c_str());
                                  return le::nullterm(); },
         "{le_call, {multiply,A,B},Tag}",
               [] (varbind& vb) { double a=vb["A"]->to_double();
                                  double b=vb["B"]->to_double();
                                  return le::fmt(vb,"{le_reply, ~f, Tag}",a*b); },
         "{stop, Reason}",
               [] (varbind& vb) {le::exit_loop();
                                  std::cerr << "Stopping becasue "
                                            << vb["Reason"]->to_string() << std::endl;
                                  return le::nullterm(); }, // returning le:nullterm() allows you to
                                                            // send nothing back to erlang
         le::anyterm(),
               [] (varbind& vb) { std::cerr << "Ignoring strange message";
                                   return le::nullterm(); }
         );

   le::enter_loop(dp);

}
```
This makes it very easy. Notice the simple statement: ```le::fmt(vb,"{le_reply,
~f, Tag}",a*b);``` `le:fmt` is doing the following:
* Creating an erlang term (that will be sent back as a reply to
  gen_exe:port_call({multiply,X,Y}))
* Puts the result of the multiplication where it finds `~f`
* Reads the `Tag` from the incoming erlang message, and resends it as part of
  the built reply

Now that is simplicity for you!! Thanks to the wonderful eixx library and a few
utility functions in the le_eixx library!

`le:enter_loop()` takes care of reading erlang terms from stdin, pattern
matching against the provided erlang patterns and dispatching the desired user
function. The above is a full working port program in a few lines!

Please note that le_eixx is not thread safe (it writes to stdout and reads from
stdin!!). It needs to be used within its own thread, but I'd rather have
non-threaded port programs and leave all concurrency to erlang if possible.

## Status
This is an early version, but the test cases are rather thorough. Documentation
is not written yet. Browse through the test cases in gen_exe_SUITE.erl for now.
