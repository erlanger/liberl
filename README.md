# liberl - Erlang application utilities and gen\_exe for port management

## Dependencies
The library depends on gproc(https://github.com/uwiger/gproc), which I find
most useful for almost any erlang application.

## Purpose
I wrote this library because I found myself writing boiler-plate code
for every application I wrote. Code to manage application options,
set debug flags, etc.

The second reason is because I have several applications whose job
is to interact with a small external C++/C executable, and I found
myself writing boiler-plate code all over again, so I decided to make
a generic port server, with a few callbacks. This is the gen\_exe 
gen\_server module.

## Status
This is a very early version, it is a work in progress. Documentation
is not written yet.
