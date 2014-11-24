#!/bin/sh

cd `dirname "$0"`
PA=../../liberl/ebin

erlc -pa "$PA"  perf_gen_exe.erl perf_plain_msg.erl perf_portcmd.erl
erl -pa "$PA" -pa ../deps/**/ebin  \
    -noinput                       \
    -run perf_gen_exe start        \
    -run perf_plain_msg start      \
    -run perf_portcmd start      \
    -run init stop
echo
