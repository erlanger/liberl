INCLUDEDIRS=-I../include -I eixx/include -I /usr/lib/erlang/lib/erl_interface-3.7.18/include/
LIBDIRS=-L /usr/lib/erlang/lib/erl_interface-3.7.18/lib/
LIBS=-lei
DEFINES=-DLE_DBG
SRC=le_eixx.cpp

le_eixx: le_eixx.cpp ../include/le/le_eixx.hpp
	g++ $(CXXFLAGS) $(DEFINES) -std=c++11 -o $@ $(INCLUDEDIRS) $(LIBDIRS) $(SRC) $(LIBS)

#Make sure le_eixx header does not cause multiple-references in linker.
test:
	echo #include \"le/le_eixx.hpp\" > test.cpp
	echo static const int k=5; > test.cpp
	g++ $(DEFINES) -std=c++11 -o $@ $(INCLUDEDIRS) $(LIBDIRS) $(SRC) test.cpp $(LIBS)

