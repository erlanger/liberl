# liberl - Erlang application utilities and gen_exe for port management

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

## gen_exe - port management 
gen_exe provides the boiler_plate code that we write over and over again every
time that we need to connect to a port program. It also takes cares of tricky
situations, such as when a port doesn't exit when stdin is closed and of 
getting the return status of the port and notifying the user of its termination.
Gen_exe provides the ability to communicate to the port program more or less like a
gen_server. It even supports calls to the port so you can do something like

```erlang
12.0=gen_exe:port_call(Pid,{multiply,3.0,4.0})
```
 or
```erlang
gen_exe:port_cast(Pid,{mymessage,"hello"})
```

The idea is to have a simple call back module that behaves in a
very similar way to a gen_server call back module. In addition to the normal
gen_server handle_call, handle_cast and handle_info events, the module receives
the following port_specific callbacks:

* `port_exit` - when the port program has finished/died for ANY reason
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
addition to processing port specific callbacks. This was a design decision to 
leverage the many years of stability and time-tested value of the gen_server.

## C++ Port programs - the easy way
liberl, will also make it much easier to write the port program in C++ if you
use the eixx library from https://github.com/saleyn/eixx. You simply need to
include the le_eixx.hpp header and away you go!

Let's say you want to write a port program in C++ that multiplies two numbers
received from the erlang side and send the reply back to erlang.

Here is how simple it is:

```C++
#include "le/le_eixx.hpp"

int main(int argc, char *argv[])
{
   using namespace eixx;
   using namespace le;

   auto dp = le::make_dispatcher(
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

This makes it very easy. Notice all the things this program is doing:

1. Pattern matching against three different patterns:

```erlang
       le_call,{multiply,A,B},Tag}, {stop, Reason} and le:anyterm()
```
2. Binding variables `A`,`B`,`Tag` and `Reason` to their respective values
3. Executing the provided user function if there is a pattern match
4. Sending the reply of the function back to erlang
5. Gracefully stopping when it receives a {stop,Reason} message
6. Printing a warning for any messages received that it does't know about

On the erlang side you can simply do something like:

```erlang
Result=gen_exe:port_call(GenExePid,{multiply,3.0,4.0}).
Result==12.0.
true

gen_exe:port_stop(GenExePid,"Bye").
```
gen_exe takes care of timeouts, making references, sending the reply to the
caller, etc. It uses gen_server facilities, so it is time-tested.

Notice this simple statement in the C++ code above, in the lambda for matching
the multiply message: 

```C++
return le::fmt(vb,"{le_reply, ~f, Tag}",a*b);
``` 

This simple statement doing the following:
* Creating an erlang tuple (that will be sent back as a reply to
  gen_exe:port_call({multiply,X,Y}))
* Puts the result of the multiplication of a*b where it finds `~f` (float substitution)
* Reads the `Tag` from the incoming erlang message, and resends it as part of
  the built reply (Tag is an opaque object that gen_exe uses to prevent mismatching 
  of messages)
* asking le to encode the reply in a binary packet  and finally
* send the reply to erlang

All this in one readable line of code! Now that is simplicity for you!!
Thanks to the wonderful eixx library and a few
utility functions in the le_eixx library!

`le:enter_loop()` takes care of reading erlang terms from stdin, pattern
matching against the provided erlang patterns and dispatching the desired user
function. The above is a full working port program in a few lines!

Please note that le_eixx is not thread safe (it writes to stdout and reads from
stdin!!). It needs to be used within its own threadr; however ,I'd rather recommend
non-threaded port programs and leave all concurrency to erlang if possible.

## Status
This is an early version, but the test cases are rather thorough. Documentation
is not written yet. Browse through the test cases in gen_exe_SUITE.erl for now.
