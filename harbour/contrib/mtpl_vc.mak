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

OBJ_ROOT = obj
OBJ_DIR = $(OBJ_ROOT)\$(HB_CC_DIRNAME)\\
LIB_DIR = $(HB_ROOT)\lib\$(HB_CC_DIRNAME)\\
BIN_DIR = $(HB_ROOT)\bin\$(HB_CC_DIRNAME)\\

INCLUDE_DIR = $(HB_ROOT)\include

#**********************************************************
# Macros to access Harbour executable and other goodies
#**********************************************************

!ifndef HB
HB = $(BIN_DIR)harbour.exe
!endif

#**********************************************************
# C compiler definition and C flags. These should never have to change.
#**********************************************************

CC     = cl.exe
LINKER = link.exe
MKLIB  = lib.exe

#**********************************************************

# In which mode compile Harbour C or CPP
!if "$(HB_BUILD_MODE)" == "cpp"
HB_BUILD_MODE  = P
!else
HB_BUILD_MODE  = C
!endif

#**********************************************************

# Visual C++ version
!ifndef HB_VISUALC_VER
HB_VISUALC_VER = 80
!endif

# C Compiler Flags
!if $(HB_VISUALC_VER) >= 80
CFLAGS_VER     = -Ot2b1 -EHs-c- -D_CRT_SECURE_NO_DEPRECATE
!else
CFLAGS_VER     = -Ogt2yb1p -GX- -G6 -YX
!endif
#-----------

CFLAGS         = -nologo -W3 -w34701 -Gs -I$(INCLUDE_DIR) $(CFLAGS_VER) -T$(HB_BUILD_MODE) \
                 $(C_USR) $(CFLAGS)

#-----------
!if "$(HB_BUILD_DEBUG)" == "yes"
CFLAGS         = -Zi $(CFLAGS)
DBGMARKER      =  d
!endif
#-----------
!if "$(HB_BUILD_ST)" != "yes"
CFLAGS         = -MT$(DBGMARKER) $(CFLAGS)
!endif
#-----------

#**********************************************************

CLIBFLAGS      = -c $(CFLAGS) $(CLIBFLAGS)
HARBOURFLAGS   = -i$(INCLUDE_DIR) -n -q0 -w3 -es2 -km $(PRG_USR) $(HARBOURFLAGS)
LDFLAGS        =  $(LDFLAGS)

#**********************************************************
# COMPILE Rules
#**********************************************************

#**********************************************************
# General *.c --> *.obj COMPILE rule for STATIC Libraries
{.}.c{$(OBJ_DIR)}$(OBJEXT)::
    if not exist "$(OBJ_DIR)" md "$(OBJ_DIR)" > nul
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#**********************************************************
# General *.cpp --> *.obj COMPILE rule for STATIC Libraries
{.}.cpp{$(OBJ_DIR)}$(OBJEXT)::
    if not exist "$(OBJ_DIR)" md "$(OBJ_DIR)" > nul
    $(CC) $(CLIBFLAGS: -TC= -TP) -Fo$(OBJ_DIR)\ $<
#**********************************************************
# General *.prg --> *.obj COMPILE rule for STATIC Libraries
{.}.prg{$(OBJ_DIR)}$(OBJEXT):
    if not exist "$(OBJ_DIR)" md "$(OBJ_DIR)" > nul
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#**********************************************************

!include common.mak

ALL_HEADERS = $(PRG_HEADERS) $(C_HEADERS)

#**********************************************************
$(LIB_PATH) : $(LIB_OBJS)
    if exist "$@" $(DEL) "$@" > nul
    $(MKLIB) /out:$@ @<<
$**
<<$(HB_KEEPSTATE)
#**********************************************************
# CLEAN rule(s)
#**********************************************************

clean: doClean
Clean: doClean
CLEAN: doClean

doClean:
    @if exist $(LIB_PATH) $(DEL) $(LIB_PATH) > nul
    @$(ECHO) @echo off                        > _hbdeloo.bat
    @$(ECHO) if %%1x == x goto SKIP          >> _hbdeloo.bat
    @$(ECHO) if exist %%1.c   $(DEL) %%1.c   >> _hbdeloo.bat
    @$(ECHO) if exist %%1.obj $(DEL) %%1.obj >> _hbdeloo.bat
    @$(ECHO) :SKIP                           >> _hbdeloo.bat
    <<_hbdeloa.bat
@call _hbdeloo.bat $(LIB_OBJS:.obj=^
@call _hbdeloo.bat )
<<KEEP
    @if exist _hbdeloa.bat $(DEL) _hbdeloa.bat > nul
    @if exist _hbdeloo.bat $(DEL) _hbdeloo.bat > nul
    @if exist "$(OBJ_DIR)" rd "$(OBJ_DIR)" > nul 2> nul
    @if exist "$(OBJ_ROOT)" rd "$(OBJ_ROOT)" > nul 2> nul

!if "$(HB_INSTALL_PREFIX)" == "$(HB_ROOT)"
    @if exist $(HB_LIB_INSTALL)\$(LIBNAME)$(LIBEXT) $(DEL) $(HB_LIB_INSTALL)\$(LIBNAME)$(LIBEXT) > nul
    @$(ECHO) @echo off                        > _hbdelho.bat
    @$(ECHO) if %%1x == x goto SKIP          >> _hbdelho.bat
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
    @$(ECHO) if %%1x == x goto SKIP                  >> _hbcpyho.bat
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
