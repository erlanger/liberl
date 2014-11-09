INCLUDEDIRS=-I../include -I eixx/include -I /usr/lib/erlang/lib/erl_interface-3.7.18/include/
LIBDIRS=-L /usr/lib/erlang/lib/erl_interface-3.7.18/lib/
LIBS=-lei
DEFINES=-DLE_DBG

le_eixx: le_eixx.cpp ../include/le/le_eixx.hpp
	g++ $(DEFINES) -std=c++11 -o $@ $(INCLUDEDIRS) $(LIBDIRS) $< $(LIBS)
