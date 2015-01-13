ERL_ROOT=/usr/lib/erlang
EI_VER:=$(shell find $(ERL_ROOT) -type d -name "erl_interface*" | sed -r 's/^[^0-9]*(([0-9]+\.)+)/\1/')
INCLUDEDIRS=-I../include -I ../deps/eixx/include -I /usr/lib/erlang/lib/erl_interface-$(EI_VER)/include/
LIBDIRS=-L /usr/lib/erlang/lib/erl_interface-$(EI_VER)/lib/
LIBS=-lei
DEFINES=#-DLE_DBG

all: le_eixx erlsay

le_eixx: le_eixx.cpp ../include/le/le_eixx.hpp
	$(CXX) $(DEFINES) $(CXXFLAGS) -std=c++11 -o $@ $(INCLUDEDIRS) $(LIBDIRS) $^ $(LIBS)

erlsay: erlsay.cpp ../include/le/le_eixx.hpp
	$(CXX) $(DEFINES) $(CXXFLAGS) -std=c++11 -o $@ $(INCLUDEDIRS) $(LIBDIRS) $^ $(LIBS)

clean:
	rm -f erlsay le_eixx

#Make sure le_eixx header does not cause multiple-references in linker.
test:
	echo #include \"le/le_eixx.hpp\" > test.cpp
	echo static const int k=5; > test.cpp
	$(CXX) $(DEFINES) -std=c++11 -o $@ $(INCLUDEDIRS) $(LIBDIRS) $(SRC) test.cpp $(LIBS)

