## This is Makefile template

HEADERS = $(SDKROOT)/openvx/include
LIB_DIR = $(SDKROOT)/openvx/lib
LIBS = -lopenvx
SOURCES = source.cpp
BIN = app
CXX = g++
CXXFLAGS = –pthread -I$(HEADERS) -L$(LIB_DIR)

all : $(SOURCES)
    $(CXX) $(CXXFLAGS) -o $(BIN) $(SOURCES) $(LIBS)
clean :
	rm $(BIN)
