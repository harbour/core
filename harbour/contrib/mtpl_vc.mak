#
# $Id$
#

# --------------------------------------------------------
# Makefile common section for Harbour Project Contrib libs
# for Microsoft Visual C
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

!ifndef HB_ROOT
HB_ROOT = ..\..
!endif

#**********************************************************

!ifndef ECHO
ECHO = echo
!endif
!ifndef DEL
DEL = del
!endif

#**********************************************************
# binary file suffixes and prefixes
#**********************************************************

!ifndef OBJEXT
OBJEXT = .obj
!endif
!ifndef LIBEXT
LIBEXT = .lib
!endif

#**********************************************************

.SUFFIXES: $(LIBEXT) $(OBJEXT) .prg .c .cpp .asm

#**********************************************************
# Install directory defaults.
#**********************************************************

!ifndef HB_INSTALL_PREFIX
HB_INSTALL_PREFIX = $(HB_ROOT)
!endif

!ifndef HB_BIN_INSTALL
HB_BIN_INSTALL = $(HB_INSTALL_PREFIX)\bin
!endif
!ifndef HB_INC_INSTALL
HB_INC_INSTALL = $(HB_INSTALL_PREFIX)\include
!endif
!ifndef HB_LIB_INSTALL
HB_LIB_INSTALL = $(HB_INSTALL_PREFIX)\lib
!endif

#**********************************************************
# Directory macros. These should never have to change.
#**********************************************************

OBJ_DIR = $(HB_ROOT)\obj\vc
LIB_DIR = $(HB_ROOT)\lib\vc
BIN_DIR = $(HB_ROOT)\bin\vc

INCLUDE_DIR = $(HB_ROOT)\include

#**********************************************************
# Macros to access Harbour executable and other goodies
#**********************************************************

HARBOUR_EXE = $(BIN_DIR)\harbour.exe

#**********************************************************
# C compiler definition and C flags. These should never have to change.
#**********************************************************

CC     = cl.exe
LINKER = link.exe
MKLIB  = lib.exe

#**********************************************************

CFLAGS         = -I$(INCLUDE_DIR) -W3 -nologo $(C_USR) $(CFLAGS)
CLIBFLAGS      = -c $(CFLAGS) $(CLIBFLAGS)
CLIBFLAGSDEBUG = -Zi $(CLIBFLAGS)
HARBOURFLAGS   = -i$(INCLUDE_DIR) -n -q0 -w2 -es2 -gc0 $(PRG_USR) $(HARBOURFLAGS)
LDFLAGS        = $(LDFLAGS)

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
#   if exist "$@" $(DEL) "$@" > nul
#   $(MKLIB) /out:$@ @<<
#$**
#<<$(KEEPSTATE)
#**********************************************************

!include common.mak

ALL_HEADERS = $(PRG_HEADERS) $(C_HEADERS)

#**********************************************************
$(LIB_PATH) : $(LIB_OBJS)
    if exist "$@" $(DEL) "$@" > nul
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
    @if exist $(LIB_PATH) $(DEL) $(LIB_PATH) > nul
    @$(ECHO) @echo off                        > _hbdeloo.bat
    @$(ECHO) if "%%1" == "" goto SKIP        >> _hbdeloo.bat
    @$(ECHO) if exist %%1.c   $(DEL) %%1.c   >> _hbdeloo.bat
    @$(ECHO) if exist %%1.obj $(DEL) %%1.obj >> _hbdeloo.bat
    @$(ECHO) :SKIP                           >> _hbdeloo.bat
    <<_hbdeloa.bat
@call _hbdeloo.bat $(LIB_OBJS:.obj=^
@call _hbdeloo.bat )
<<KEEP
    @if exist _hbdeloa.bat $(DEL) _hbdeloa.bat > nul
    @if exist _hbdeloo.bat $(DEL) _hbdeloo.bat > nul

!if "$(HB_INSTALL_PREFIX)" == "$(HB_ROOT)"
    @if exist $(HB_LIB_INSTALL)\$(LIBNAME)$(LIBEXT) $(DEL) $(HB_LIB_INSTALL)\$(LIBNAME)$(LIBEXT) > nul
    @$(ECHO) @echo off                        > _hbdelho.bat
    @$(ECHO) if "%%1" == "" goto SKIP        >> _hbdelho.bat
    @$(ECHO) if exist $(HB_INC_INSTALL)\%%1 $(DEL) $(HB_INC_INSTALL)\%%1 >> _hbdelho.bat
    @$(ECHO) :SKIP                           >> _hbdelho.bat
    <<_hbdelha.bat
@call _hbdelho.bat $(ALL_HEADERS: =^
@call _hbdelho.bat )
<<KEEP
    @if exist _hbdelha.bat $(DEL) _hbdelha.bat > nul
    @if exist _hbdelho.bat $(DEL) _hbdelho.bat > nul
!endif

#**********************************************************
# INSTALL rule(s)
#**********************************************************

install: doInstall
Install: doInstall
INSTALL: doInstall

doInstall:
    <<_hbcpyla.bat
@echo off
if not exist $(LIB_PATH) goto SKIP
if exist $(HB_LIB_INSTALL)\$(LIBNAME)$(LIBEXT) $(DEL) $(HB_LIB_INSTALL)\$(LIBNAME)$(LIBEXT) > nul
copy $(LIB_PATH) $(HB_LIB_INSTALL) > nul
:SKIP
<<KEEP
    @_hbcpyla.bat
    @if exist _hbcpyla.bat $(DEL) _hbcpyla.bat > nul
    @$(ECHO) @echo off                                > _hbcpyho.bat
    @$(ECHO) if "%%1" == "" goto SKIP                >> _hbcpyho.bat
    @$(ECHO) if exist $(HB_INC_INSTALL)\%%1 $(DEL) $(HB_INC_INSTALL)\%%1 >> _hbcpyho.bat
    @$(ECHO) if exist %%1 copy %%1 $(HB_INC_INSTALL) >> _hbcpyho.bat
    @$(ECHO) :SKIP                                   >> _hbcpyho.bat
    <<_hbcpyha.bat
@call _hbcpyho.bat $(ALL_HEADERS: =^
@call _hbcpyho.bat )
<<KEEP
    @if exist _hbcpyha.bat $(DEL) _hbcpyha.bat > nul
    @if exist _hbcpyho.bat $(DEL) _hbcpyho.bat > nul

#**********************************************************
