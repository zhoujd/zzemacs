### Makefile sample for C/C++ on Linux
### Any question contact to zjd-405@163.com


TARGET = sample

CC = gcc
CXX = g++
CXXFLAGS = -g -Wall -fPIC -msse4

CFLAGS = -g -O0 -Wall \
	-Icommon \
	-Iinclude

SOURCE_FILES = \
	src/main.c \
	common/common.c \
	common/test.c

_OBJ = $(patsubst %.c, %.o, $(SOURCE_FILES))

ODIR = obj
OBJ = $(patsubst %, $(ODIR)/%, $(_OBJ))

LIBS_PATH = 
LIBS = 

.PHONY: all
all: $(TARGET)

$(TARGET): $(OBJ)
	@mkdir -p $(ODIR)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS_PATH) $(LIBS)
	@echo "make $@ finished on `date`"

$(ODIR)/%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

.PHONY: clean
clean:
	rm -f $(ODIR)/*.o *~ core $(INCDIR)/*~
