# compiler macros
CC=gcc
CFLAGS=-Wall -g -DDEBUG -DNO_OBJ -I$(INCLUDE_DIR) -L$(LIB_DIR) -x c
CFLAGS1=-Wall -g -DDEBUG -DNO_OBJ -I$(INCLUDE_DIR) -L$(LIB_DIR)

# directory macros - define the LIB_DIR for you
INCLUDE_DIR=../../INCLUDE/
BIN_DIR=../../bin
LIB_DIR=../../LIBS/WIN32

# makefile macros
OBJECTS=$(SRCS:.PRG=.o)
COMPILE.C=$(COMPILE.c)

# default targets

all:

clean:
	-rm -Rf $(BINARY) core *~ y_tab.* lexyy.c y.output *.o *.exe

.prg.c:
	$(BIN_DIR)/harbour $? /N

.PRG.c:
	$(BIN_DIR)/harbour $? /N

.o.exe:
	$(CC) $(CFLAGS1) -o $@ $? -lharbour

.SUFFIXES: .prg .PRG .exe .EXE
