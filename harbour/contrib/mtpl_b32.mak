#
# $Id$
#

# --------------------------------------------------------
# Makefile common section for Harbour Project Contrib libs
# for Borland C/C++
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

.AUTODEPEND
.SUFFIXES:

#**********************************************************

HB_ARCHITECTURE = w32

#**********************************************************

!ifndef HB_ROOT
HB_ROOT = ..\..
!endif

#**********************************************************

!ifndef ECHO
ECHO = echo.
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

!ifndef HB_CC_DIRNAME
HB_CC_DIRNAME = $(_HB_CC_NAME)
!endif

OBJ_DIR = $(HB_ROOT)\obj\$(HB_CC_DIRNAME)\\
LIB_DIR = $(HB_ROOT)\lib\$(HB_CC_DIRNAME)\\
BIN_DIR = $(HB_ROOT)\bin\$(HB_CC_DIRNAME)\\

INCLUDE_DIR = $(HB_ROOT)\include

#**********************************************************
# Macros to access Harbour executable and other goodies
#**********************************************************

HARBOUR_EXE = $(BIN_DIR)harbour.exe

#**********************************************************
# C compiler definition and C flags. These should never have to change.
#**********************************************************

CC     = bcc32.exe
LINKER = ilink32.exe
MKLIB  = tlib.exe

#**********************************************************

# BORLAND has ST mode as default
!if "$(HB_BUILD_ST)" == ""
    HB_BUILD_ST = yes
!endif

#**********************************************************

# In which mode compile Harbour C or CPP
!if "$(HB_BUILD_MODE)" == "cpp"
HB_BUILD_MODE  = -P
!else
HB_BUILD_MODE  =
!endif

#**********************************************************

CFLAGS = -I$(INCLUDE_DIR) -d $(C_USR) $(CFLAGS) $(HB_BUILD_MODE)

#-----------
!if "$(HB_BUILD_DEBUG)" == "yes"
    CFLAGS  = -y -v $(CFLAGS)
!endif
#-----------
!if !$d(BCC_NOOPTIM)
    CFLAGS = -O2 $(CFLAGS)
!endif
#-----------
!if "$(HB_BUILD_ST)" != "yes"
    CFLAGS  = -tWM $(CFLAGS)
!endif
#-----------

#**********************************************************

CLIBFLAGS      = -c -q $(CFLAGS) $(CLIBFLAGS)
HARBOURFLAGS   = -i$(INCLUDE_DIR) -n -q0 -w3 -es2 -gc0 $(PRG_USR) $(HARBOURFLAGS)
LDFLAGS        =  $(LDFLAGS)

#**********************************************************

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
    $(CC) $(CLIBFLAGS) -o$@ $<
#**********************************************************
# General *.cpp --> *.obj COMPILE rule for STATIC Libraries
{$(ALL_LIB_SRC_DIRS)}.cpp{$(OBJ_DIR)}$(OBJEXT):
    $(CC) $(CLIBFLAGS: -P= ) -P -o$@ $<
#**********************************************************
# General *.prg --> *.obj COMPILE rule for STATIC Libraries
{$(ALL_LIB_SRC_DIRS)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HARBOUR_EXE) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $**
    $(CC) $(CLIBFLAGS) -P -o$@ $(OBJ_DIR)\$&.c
#**********************************************************

!include common.mak

ALL_HEADERS = $(PRG_HEADERS) $(C_HEADERS)

#**********************************************************
$(LIB_PATH) : $(LIB_OBJS)
    if exist "$(LIB_PATH)" $(DEL) "$(LIB_PATH)" > NUL
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
    @if exist $(LIB_PATH) $(DEL) $(LIB_PATH) > nul
    @$(ECHO) @echo off                      > _hbdeloo.bat
    @$(ECHO) if %1x == x goto SKIP         >> _hbdeloo.bat
    @$(ECHO) if exist %1.c   $(DEL) %1.c   >> _hbdeloo.bat
    @$(ECHO) if exist %1.obj $(DEL) %1.obj >> _hbdeloo.bat
    @$(ECHO) :SKIP                         >> _hbdeloo.bat
    @type &&!
@call _hbdeloo.bat $(LIB_OBJS:.obj=^
@call _hbdeloo.bat )
! > _hbdeloa.bat
    @_hbdeloa.bat
    @if exist _hbdeloa.bat $(DEL) _hbdeloa.bat > nul
    @if exist _hbdeloo.bat $(DEL) _hbdeloo.bat > nul

!if "$(HB_INSTALL_PREFIX)" == "$(HB_ROOT)"
    @if exist $(HB_LIB_INSTALL)\$(LIBNAME)$(LIBEXT) $(DEL) $(HB_LIB_INSTALL)\$(LIBNAME)$(LIBEXT) > nul
!endif
    @$(ECHO) @echo off                                                  > _hbdelho.bat
    @$(ECHO) if %1x == x goto SKIP                                     >> _hbdelho.bat
    @$(ECHO) if $(HB_INSTALL_PREFIX)x == $(HB_ROOT)x goto SKIP         >> _hbdelho.bat
    @$(ECHO) if exist $(HB_INC_INSTALL)\%1 $(DEL) $(HB_INC_INSTALL)\%1 >> _hbdelho.bat
    @$(ECHO) :SKIP                                                     >> _hbdelho.bat
    @type &&!
@call _hbdelho.bat $(ALL_HEADERS: =^
@call _hbdelho.bat )
! > _hbdelha.bat
    @_hbdelha.bat
    @if exist _hbdelha.bat $(DEL) _hbdelha.bat > nul
    @if exist _hbdelho.bat $(DEL) _hbdelho.bat > nul

#**********************************************************
# INSTALL rule(s)
#**********************************************************

install: doInstall
Install: doInstall
INSTALL: doInstall

doInstall:
    @type &&!
@echo off
if not exist $(LIB_PATH) goto SKIP
if exist $(HB_LIB_INSTALL)\$(LIBNAME)$(LIBEXT) $(DEL) $(HB_LIB_INSTALL)\$(LIBNAME)$(LIBEXT) > nul
copy $(LIB_PATH) $(HB_LIB_INSTALL) > nul
:SKIP
! > _hbcpyla.bat
    @_hbcpyla.bat
    @if exist _hbcpyla.bat $(DEL) _hbcpyla.bat > nul
    @$(ECHO) @echo off                                                  > _hbcpyho.bat
    @$(ECHO) if %1x == x goto SKIP                                     >> _hbcpyho.bat
    @$(ECHO) if exist $(HB_INC_INSTALL)\%1 $(DEL) $(HB_INC_INSTALL)\%1 >> _hbcpyho.bat
    @$(ECHO) if exist %1 copy %1 $(HB_INC_INSTALL)                     >> _hbcpyho.bat
    @$(ECHO) :SKIP                                                     >> _hbcpyho.bat
    @type &&!
@call _hbcpyho.bat $(ALL_HEADERS: =^
@call _hbcpyho.bat )
! > _hbcpyha.bat
    @_hbcpyha.bat
    @if exist _hbcpyha.bat $(DEL) _hbcpyha.bat > nul
    @if exist _hbcpyho.bat $(DEL) _hbcpyho.bat > nul

#**********************************************************
