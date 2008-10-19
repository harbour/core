#
# $Id$
#

# --------------------------------------------------------
# Makefile common section for Harbour Project Contrib libs
# for GNU gcc compiler
# --------------------------------------------------------

# ---------------------------------------------------------------
# Copyright 2007 Marek Paliwoda (mpaliwoda "at" interia "dot" pl)
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

# NOTE: You can use these optional envvars to configure the make process:
#
#       C_USR        - Extra C compiler options for libraries
#       PRG_USR      - Extra Harbour compiler options
#

#**********************************************************

ifndef HB_ROOT
HB_ROOT = ../..
endif

#**********************************************************

ifndef ECHO
ECHO = echo
endif
ifndef DEL
DEL = rm -f
endif
ifndef COPY
COPY = cp
endif

#**********************************************************
# binary file suffixes and prefixes
#**********************************************************

ifndef OBJEXT
OBJEXT = .o
endif
ifndef LIBEXT
LIBEXT = .a
endif
ifndef LIBPREF
LIBPREF=lib
endif

#**********************************************************

.SUFFIXES: $(LIBEXT) $(OBJEXT) .prg .c .cpp .asm

#**********************************************************
# Install directory defaults.
#**********************************************************

ifndef HB_INSTALL_PREFIX
HB_INSTALL_PREFIX = $(HB_ROOT)
endif

ifndef HB_BIN_INSTALL
HB_BIN_INSTALL = $(HB_INSTALL_PREFIX)/bin
endif
ifndef HB_INC_INSTALL
HB_INC_INSTALL = $(HB_INSTALL_PREFIX)/include
endif
ifndef HB_LIB_INSTALL
HB_LIB_INSTALL = $(HB_INSTALL_PREFIX)/lib
endif

#**********************************************************
# Directory macros. These should never have to change.
#**********************************************************

ifndef HB_CC_DIRNAME
HB_CC_DIRNAME = $(_HB_CC_NAME)
endif

OBJ_DIR = obj/$(HB_CC_DIRNAME)/
LIB_DIR = $(HB_ROOT)/lib/$(HB_CC_DIRNAME)/
BIN_DIR = $(HB_ROOT)/bin/$(HB_CC_DIRNAME)/

INCLUDE_DIR = $(HB_ROOT)/include

#**********************************************************
# Macros to access Harbour executable and other goodies
#**********************************************************

ifndef HB
HB = $(BIN_DIR)harbour
endif

#**********************************************************
# C compiler definition and C flags. These should never have to change.
#**********************************************************

# CC and LD are set in make_gcc.sh

#ifeq ($(CC),)
#CC = gcc
#endif
#ifeq ($(LD),)
#LD = gcc
#endif
ifeq ($(MKLIB),)
MKLIB = ar
endif

#**********************************************************

# C Compiler Flags

ifndef GCC_NOOPTIM
CFLAGS         := -O3 $(CFLAGS)
endif

CFLAGS         := -W -Wall -I$(INCLUDE_DIR) $(C_USR) $(CFLAGS)
CLIBFLAGS      := -c $(CFLAGS) $(CLIBFLAGS)
HARBOURFLAGS   := -i$(INCLUDE_DIR) -n -q0 -w3 -es2 -km -l $(PRG_USR) $(HARBOURFLAGS)
ifeq ($(HB_BUILD_DEBUG),yes)
HARBOURFLAGS   := $(HARBOURFLAGS) -l-
endif
LDFLAGS        := $(LDFLAGS)

#**********************************************************
# COMPILE Rules
#**********************************************************

#**********************************************************
# General *.c --> *.obj COMPILE rule for STATIC Libraries
$(OBJ_DIR)%$(OBJEXT) : %.c
	$(CC) $(CLIBFLAGS) -o$@ $<
#**********************************************************
# General *.cpp --> *.obj COMPILE rule for STATIC Libraries
$(OBJ_DIR)%$(OBJEXT) : %.cpp
	$(CXX) $(CLIBFLAGS) -o$@ $<
#**********************************************************
# General *.prg --> *.obj COMPILE rule for STATIC Libraries
$(OBJ_DIR)%$(OBJEXT) : %.prg
	$(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)/ $<
	$(CC) $(CLIBFLAGS) -o$@ $(OBJ_DIR)/$(<F:.prg=.c)
#**********************************************************

include common.mak

ALL_HEADERS = $(PRG_HEADERS) $(C_HEADERS)

#**********************************************************
$(LIB_PATH) : $(LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
# CLEAN rule(s)
#**********************************************************

clean: doClean
Clean: doClean
CLEAN: doClean

doClean:
	-@if [ -f $(LIB_PATH) ]; then $(DEL) $(LIB_PATH); fi
	-@if [ "$(HB_INSTALL_PREFIX)" = "$(HB_ROOT)" ]; then     \
	   if [ -f $(HB_LIB_INSTALL)/$(LIBNAME)$(LIBEXT) ]; then \
	     $(DEL) $(HB_LIB_INSTALL)/$(LIBNAME)$(LIBEXT);       \
	   fi;                                                   \
	 fi
	-@for OBJ in $(LIB_OBJS) dummy.file; do        \
	   if [ -f $${OBJ} ]; then $(DEL) $${OBJ}; fi; \
	 done
	-@for HDR in $(ALL_HEADERS) dummy.file; do     \
	   if [ -f $${HB_INC_INSTALL}/$${HDR} ]; then  \
	     $(DEL) $${HB_INC_INSTALL}/$${HDR};        \
	   fi;                                         \
	 done

#**********************************************************
# INSTALL rule(s)
#**********************************************************

install: doInstall
Install: doInstall
INSTALL: doInstall

doInstall:
	-@if [ -f $(LIB_PATH) ]; then $(COPY) $(LIB_PATH) $(HB_LIB_INSTALL); fi
	-@for HDR in $(ALL_HEADERS) dummy.file; do   \
	   if [ -f $${HDR} ]; then $(COPY) $${HDR} $(HB_INC_INSTALL); fi; \
	 done

#**********************************************************
