#
# $Id$
#

# -------------------------------------------------------------------
# Copyright 2007 Marek Paliwoda (mpaliwoda "at" interia "dot" pl)
# Copyright 2007 Viktor Szakats (viktor.szakats "at" syenar "dot" hu)
# See doc/license.txt for licensing terms.
# -------------------------------------------------------------------

# --------------------------------------------------------
# Makefile common section for Harbour Project Contrib libs
# for Borland C/C++
# --------------------------------------------------------

# NOTE: You can use these envvars to configure the make process:
#   	(note that these are all optional)
#
#   	CFLAGS  		 - Extra C compiler options for libraries and for
#   							   executables
#   	C_USR   		 - Extra C compiler options for libraries and for
#   							   executables (GNU make compatible envvar)
#   	CLIBFLAGS    - Extra C compiler options for the libraries
#   	HARBOURFLAGS - Extra Harbour compiler options
#   	PRG_USR 		 - Extra Harbour compiler options
#   							   (GNU make compatible envvar)

#.KEEP
.AUTODEPEND
.SUFFIXES:

#**********************************************************

!ifndef HB_ROOT
HB_ROOT = ..\..
!endif

!include $(HB_ROOT)\contrib\mtpl_defs.mak

#**********************************************************
# C compiler definition and C flags. These should never have to change.
#**********************************************************

CC     = bcc32.exe
LINKER = ilink32.exe
MKLIB  = tlib.exe

#**********************************************************

!if !$d(BCC_NOOPTIM)
CFLAGS = -O2 $(CFLAGS)
!endif

CFLAGS  	   = -I$(INCLUDE_DIR) -d $(C_USR) $(CFLAGS)
CLIBFLAGS      = -c $(CFLAGS) $(CLIBFLAGS)
CLIBFLAGSDEBUG = -v $(CLIBFLAGS)
HARBOURFLAGS   = -i$(INCLUDE_DIR) -n -q0 -w2 -es2 -gc0 $(PRG_USR) $(HARBOURFLAGS)
LDFLAGS 	   = $(LDFLAGS)

# This is needed, otherwise the libs may overflow when
# debug info is requested with -v -y
ARFLAGS = /P32

#**********************************************************
# COMPILE Rules
#**********************************************************

ALL_LIB_SRC_DIRS_TMP=\
.;\
$(OBJ_DIR);\

#**********************************************************
# General *.c --> *.obj COMPILE rule for STATIC Libraries
{$(ALL_LIB_SRC_DIRS)}.c{$(OBJ_DIR)}$(OBJEXT):
   echo $(CC) $(CLIBFLAGS) -o$@ $<
   $(CC) $(CLIBFLAGS) -o$@ $<
#**********************************************************
# General *.cpp --> *.obj COMPILE rule for STATIC Libraries
{$(ALL_LIB_SRC_DIRS)}.cpp{$(OBJ_DIR)}$(OBJEXT):
   echo $(CC) $(CLIBFLAGS) -o$@ $<
   $(CC) $(CLIBFLAGS) -o$@ $<
#**********************************************************
# General *.prg --> *.obj COMPILE rule for STATIC Libraries
{$(ALL_LIB_SRC_DIRS)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
   $(HARBOUR_EXE) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $**
   $(CC) $(CLIBFLAGS) -o$@ $(OBJ_DIR)\$&.c
#**********************************************************
# General Library BUILD rule - does not work
#{$(OBJ_DIR)}.obj{$(LIB_DIR)}.lib:
#   IF EXIST "$@" $(DEL) "$@" > NUL
#   $(MKLIB) "$@" $(ARFLAGS) @&&!
#+$(**: = &^
#+)
#!
#**********************************************************

!include common.mak

#**********************************************************
$(LIB_PATH) : $(LIB_OBJS)
   IF EXIST "$(LIB_PATH)" $(DEL) "$(LIB_PATH)" > NUL
   $(MKLIB) "$(LIB_PATH)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************

!include $(HB_ROOT)\contrib\mtpl_ruls.mak
