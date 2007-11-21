#
# $Id$
#

# --------------------------------------------------------
# Makefile common section for Harbour Project Contrib libs
# for Microsoft Visual C
# --------------------------------------------------------

# ---------------------------------------------------------------
# Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

# NOTE: You can use these envvars to configure the make process:
#   	(note that these are all optional)
#
#   	CFLAGS  				 - Extra C compiler options for libraries and for
#   													   executables
#   	C_USR   				 - Extra C compiler options for libraries and for
#   													   executables (GNU make compatible envvar)
#   	CLIBFLAGS    - Extra C compiler options for the libraries
#   	HARBOURFLAGS - Extra Harbour compiler options
#   	PRG_USR 				 - Extra Harbour compiler options
#   													   (GNU make compatible envvar)

# Visual C++ version
!ifndef HB_VISUALC_VER
HB_VISUALC_VER = 60
!endif

#**********************************************************

!ifndef HB_ROOT
HB_ROOT = ..\..
!endif

!include $(HB_ROOT)\contrib\mtpl_defs.mak

#**********************************************************
# C compiler definition and C flags. These should never have to change.
#**********************************************************

CC     = cl.exe
LINKER = link.exe
MKLIB  = lib.exe

#**********************************************************

CFLAGS  		   = -I$(INCLUDE_DIR) -W3 -nologo $(C_USR) $(CFLAGS)
CLIBFLAGS      = -c $(CFLAGS) $(CLIBFLAGS)
CLIBFLAGSDEBUG = -Zi $(CLIBFLAGS)
HARBOURFLAGS   = -i$(INCLUDE_DIR) -n -q0 -w2 -es2 -gc0 $(PRG_USR) $(HARBOURFLAGS)
LDFLAGS 		   = $(LDFLAGS)

#**********************************************************
# COMPILE Rules
#**********************************************************

#**********************************************************
# General *.c --> *.obj COMPILE rule for STATIC Libraries
{.}.c{$(OBJ_DIR)}$(OBJEXT)::
   $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#**********************************************************
# General *.cpp --> *.obj COMPILE rule for STATIC Libraries
{.}.cpp{$(OBJ_DIR)}$(OBJEXT)::
   $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#**********************************************************
# General *.prg --> *.obj COMPILE rule for STATIC Libraries
{.}.prg{$(OBJ_DIR)}$(OBJEXT):
   $(HARBOUR_EXE) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
   $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#**********************************************************
# General Library BUILD rule - does not work
#{$(OBJ_DIR)}$(OBJEXT){$(LIB_PATH)}.lib:
#   IF EXIST "$@" $(DEL) "$@" > nul
#   $(MKLIB) /out:$@ @<<
#$**
#<<$(KEEPSTATE)
#**********************************************************

!include common.mak

ALL_HEADERS = $(PRG_HEADERS) $(C_HEADERS)

#**********************************************************
$(LIB_PATH) : $(LIB_OBJS)
   IF EXIST "$@" $(DEL) "$@" > nul
   $(MKLIB) /out:$@ @<<
$**
<<$(KEEPSTATE)
#**********************************************************
# CLEAN rule(s)
#**********************************************************

clean: doClean
Clean: doClean
CLEAN: doClean

doClean:
   @if exist $(LIB_PATH) $(DEL) $(LIB_PATH)   > nul
   @$(ECHO) @echo off   										  > delone.bat
   @$(ECHO) set >>setenv										 >> delone.bat
   @$(ECHO) if """%%1""" == """""" goto skip >> delone.bat
   @$(ECHO) if exist %%1.c   $(DEL) %%1.c    >> delone.bat
   @$(ECHO) if exist %%1.obj $(DEL) %%1.obj  >> delone.bat
   @$(ECHO) :skip   											 >> delone.bat
   @<<delall.bat
@%COMSPEC% /c delone.bat $(LIB_OBJS:.obj=^
@%COMSPEC% /c delone.bat )
<<KEEP
   @if exist delall.bat $(DEL) delall.bat > nul
   @if exist delone.bat $(DEL) delone.bat > nul
   @if exist delall.bat $(DEL) delall.bat > nul

!if "$(HB_INSTALL_PREFIX)" == "$(HB_ROOT)"
   @if exist $(HB_LIB_INSTALL)\$(LIBNAME)$(LIBEXT) $(DEL) $(HB_LIB_INSTALL)\$(LIBNAME)$(LIBEXT) > nul
   @$(ECHO) @echo off   										  > delone.bat
   @$(ECHO) if """%%1""" == """""" goto skip >> delone.bat
   @$(ECHO) if exist $(HB_INC_INSTALL)\%%1 $(DEL) $(HB_INC_INSTALL)\%%1 >> delone.bat
   @$(ECHO) :skip   											 >> delone.bat
   @<<delall.bat
@%COMSPEC% /c delone.bat $(ALL_HEADERS: =^
@%COMSPEC% /c delone.bat )
<<KEEP
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
   @$(ECHO) @echo off   														 > cpyone.bat
   @$(ECHO) if """%%1""" == """""" goto skip			>> cpyone.bat
   @$(ECHO) if exist %%1 copy %%1 $(HB_INC_INSTALL) >> cpyone.bat
   @$(ECHO) :skip   															>> cpyone.bat
   @<<cpyall.bat
@%COMSPEC% /c cpyone.bat $(ALL_HEADERS: =^
@%COMSPEC% /c cpyone.bat )
<<KEEP
   @if exist cpyall.bat $(DEL) cpyall.bat > nul
   @if exist cpyone.bat $(DEL) cpyone.bat > nul
   @if exist cpyall.bat $(DEL) cpyall.bat > nul

#**********************************************************
