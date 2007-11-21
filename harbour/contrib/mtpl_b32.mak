#
# $Id$
#

# --------------------------------------------------------
# Makefile common section for Harbour Project Contrib libs
# for Borland C/C++
# --------------------------------------------------------

# ---------------------------------------------------------------
# Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

# NOTE: You can use these envvars to configure the make process:
#       (note that these are all optional)
#
#       CFLAGS                                   - Extra C compiler options for libraries and for
#                                                                                                          executables
#       C_USR                                    - Extra C compiler options for libraries and for
#                                                                                                          executables (GNU make compatible envvar)
#       CLIBFLAGS    - Extra C compiler options for the libraries
#       HARBOURFLAGS - Extra Harbour compiler options
#       PRG_USR                                  - Extra Harbour compiler options
#                                                                                                          (GNU make compatible envvar)

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

CFLAGS                     = -I$(INCLUDE_DIR) -d $(C_USR) $(CFLAGS)
CLIBFLAGS      = -c $(CFLAGS) $(CLIBFLAGS)
CLIBFLAGSDEBUG = -v $(CLIBFLAGS)
HARBOURFLAGS   = -i$(INCLUDE_DIR) -n -q0 -w2 -es2 -gc0 $(PRG_USR) $(HARBOURFLAGS)
LDFLAGS                    = $(LDFLAGS)

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

ALL_HEADERS = $(PRG_HEADERS) $(C_HEADERS)

#**********************************************************
$(LIB_PATH) : $(LIB_OBJS)
   IF EXIST "$(LIB_PATH)" $(DEL) "$(LIB_PATH)" > NUL
   $(MKLIB) "$(LIB_PATH)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
# CLEAN rule(s)
#**********************************************************

clean: doClean
Clean: doClean
CLEAN: doClean

doClean:
   @if exist $(LIB_PATH) $(DEL) $(LIB_PATH)   > nul
   @$(ECHO) @echo off                                                                                     > delone.bat
   @$(ECHO) set >>setenv                                                                                 >> delone.bat
   @$(ECHO) if """%%1""" == """""" goto skip >> delone.bat
   @$(ECHO) if exist %%1.c   $(DEL) %%1.c    >> delone.bat
   @$(ECHO) if exist %%1.obj $(DEL) %%1.obj  >> delone.bat
   @$(ECHO) :skip                                                                                        >> delone.bat
   @type &&!
@%%COMSPEC%% /c delone.bat $(LIB_OBJS:.obj=^
@%%COMSPEC%% /c delone.bat )
! > delall.bat
   @if exist delall.bat $(DEL) delall.bat > nul
   @if exist delone.bat $(DEL) delone.bat > nul
   @if exist delall.bat $(DEL) delall.bat > nul

!if "$(HB_INSTALL_PREFIX)" == "$(HB_ROOT)"
   @if exist $(HB_LIB_INSTALL)\$(LIBNAME)$(LIBEXT) $(DEL) $(HB_LIB_INSTALL)\$(LIBNAME)$(LIBEXT) > nul
   @$(ECHO) @echo off                                                                                     > delone.bat
   @$(ECHO) if """%%1""" == """""" goto skip >> delone.bat
   @$(ECHO) if exist $(HB_INC_INSTALL)\%%1 $(DEL) $(HB_INC_INSTALL)\%%1 >> delone.bat
   @$(ECHO) :skip                                                                                        >> delone.bat
   @type &&!
@%%COMSPEC%% /c delone.bat $(ALL_HEADERS: =^
@%%COMSPEC%% /c delone.bat )
! > delall.bat
   @if exist delall.bat $(DEL) delall.bat > nul
   @if exist delone.bat $(DEL) delone.bat > nul
   @if exist delall.bat $(DEL) delall.bat > nul
!endif

#**********************************************************
# INSTALL rule(s)
#**********************************************************

install: doInstall
Install: doInstall
INSTALL: doInstall

doInstall:
   @if exist $(LIB_PATH) copy $(LIB_PATH) $(HB_LIB_INSTALL) > nul
   @$(ECHO) @echo off                                                                                                                    > cpyone.bat
   @$(ECHO) if """%%1""" == """""" goto skip                    >> cpyone.bat
   @$(ECHO) if exist %%1 copy %%1 $(HB_INC_INSTALL) >> cpyone.bat
   @$(ECHO) :skip                                                                                                                       >> cpyone.bat
   @<<cpyall.bat
@%%COMSPEC%% /c cpyone.bat $(ALL_HEADERS: =^
@%%COMSPEC%% /c cpyone.bat )
<<KEEP
   @if exist cpyall.bat $(DEL) cpyall.bat > nul
   @if exist cpyone.bat $(DEL) cpyone.bat > nul
   @if exist cpyall.bat $(DEL) cpyall.bat > nul

#**********************************************************
