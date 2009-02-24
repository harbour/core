#
# $Id$
#

#**********************************************************
# Makefile for Harbour Project for Borland C/C++ 5.x compilers
#**********************************************************

# ---------------------------------------------------------------
# Copyright 2007 Marek Paliwoda (mpaliwoda "at" interia "dot" pl)
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

# ---------------------------------------------------------------
# If you need to set additional compiler/linker options use the
# environment variables below, but please DON'T MODIFY THIS FILE
# for this purpose.
# ---------------------------------------------------------------

#
# NOTE: You can use these envvars to configure the make process:
#       (note that these are all optional)
#
#       HB_USER_CFLAGS    - Extra C compiler options for libraries and for executables
#       HB_USER_LDFLAGS   - Extra linker options for libraries
#       HB_USER_PRGFLAGS  - Extra Harbour compiler options
#
#       HB_BUILD_DLL      - If set to yes enables building harbour VM+RTL
#                           dll in addition to normal static build
#       HB_BUILD_DEBUG    - If set to yes causes to compile with debug info
#       HB_BUILD_VERBOSE  - Controls echoing commands being executed
#       HB_BUILD_OPTIM    - Setting it to 'no' disables compiler optimizations
#       HB_REBUILD_PARSER - If set to yes force preprocessing new rules by
#                           Bison (you must use Bison 2.3 or later)
#       HB_INSTALL_PREFIX - Path to installation directory into which
#                           Harbour will be installed when using 'install'
#                           mode. Defaults to current directory
#
#       HB_BCCDLL_DYNRT   - If set to -tWR causes that harbour-bc.dll
#                           will use dynamic runtime library (recommended)

#**********************************************************

#.KEEP
.AUTODEPEND
.SUFFIXES:

#**********************************************************

HB_ARCHITECTURE = win

#**********************************************************

CC     = bcc32.exe
LINKER = ilink32.exe
MKLIB  = tlib.exe

#**********************************************************

# Include Common Object list files
# shared between MSVC and Borland

!include common.mak

#**********************************************************

.SUFFIXES: $(EXEEXT) $(LIBEXT) $(OBJEXT) .prg .c .l .y

#**********************************************************

# Some definitions cannot be kept in common.mak
# due to serious limitations of Microsoft Nmake

VMMTDLL_LIB_OBJS = $(VM_DLL_OBJS:$(OBJ_DIR)=$(MTDLL_OBJ_DIR))
VMMT_LIB_OBJS = $(VM_LIB_OBJS:$(OBJ_DIR)=$(MT_OBJ_DIR))

DLL_OBJS = $(TMP_DLL_OBJS:$(OBJ_DIR)=$(DLL_OBJ_DIR)) $(VM_DLL_OBJS:$(OBJ_DIR)=$(DLL_OBJ_DIR))
MTDLL_OBJS = $(TMP_DLL_OBJS:$(OBJ_DIR)=$(DLL_OBJ_DIR)) $(VMMTDLL_LIB_OBJS)

#**********************************************************
# C compiler, Harbour compiler and Linker flags.
#**********************************************************

# C Compiler Flags
CFLAGS = -I$(INCLUDE_DIR) -I$(OBJ_DIR) $(HB_USER_CFLAGS)
CFLAGSMT = -DHB_MT_VM

#-----------
!if "$(HB_BUILD_OPTIM)" != "no"
    CFLAGS = -4 -O2 -OS -Ov -Oi -Oc $(CFLAGS)
!endif
#-----------
!if "$(HB_BUILD_DEBUG)" == "yes"
    CFLAGS = -y -v -DHB_TR_LEVEL_DEBUG $(CFLAGS)
!endif
#-----------
!if "$(HB_BCCDLL_DYNRT)" == "-tWR"
    RTLIBSUFFIX = i
!endif
#-----------

#**********************************************************

CLIBFLAGS      = -c -q -d -Q -w -w-sig- -tWM $(CFLAGS)
CLIBFLAGSDLL   = $(HB_BCCDLL_DYNRT) $(CLIBFLAGS) -DHB_DYNLIB
CEXEFLAGSDLL   = $(HB_BCCDLL_DYNRT) $(CLIBFLAGS)

#**********************************************************

# Linker Flags
LDFLAGS        = -Gn -C -ap -Tpe -L$(LIB_DIR) -L$(BIN_DIR) $(HB_USER_LDFLAGS)
LDFLAGSDLL     = -Gn -C -aa -Tpd -Gi -L$(LIB_DIR) $(HB_USER_LDFLAGS)
!if "$(HB_BUILD_DEBUG)" == "yes"
    LDFLAGS = -v $(LDFLAGS)
    LDFLAGSDLL = -v $(LDFLAGSDLL)
!endif

#**********************************************************

STANDARD_STATIC_CLIBS = cw32mt$(RTLIBSUFFIX).lib

# This is needed, otherwise the libs may overflow
# when debug info is requested with -v -y
ARFLAGS = /P64 $(HB_USER_AFLAGS)

#**********************************************************
#**********************************************************
#**********************************************************

#**********************************************************
# COMPILE Rules
#**********************************************************

#**********************************************************
# General *.c --> *.obj COMPILE rule for STATIC Libraries
{$(ALL_SRC_DIRS)}.c{$(OBJ_DIR)}$(OBJEXT):
    $(CC) $(CLIBFLAGS) -o$@ $<
#**********************************************************
# General *.prg --> *.obj COMPILE rule for STATIC Libraries
{$(ALL_SRC_DIRS)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSLIB) -o$(OBJ_DIR)\ $**
    $(CC) $(CLIBFLAGS) -o$@ $(OBJ_DIR)\$&.c
#**********************************************************
# General *.prg --> *.obj COMPILE rule for STATIC exe
{$(ALL_EXE_SRC_DIRS)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSEXE) -o$(OBJ_DIR)\ $**
    $(CC) $(CLIBFLAGS) -o$@ $(OBJ_DIR)\$&.c
#**********************************************************

#**********************************************************
# General *.c --> *.obj COMPILE rule for STATIC MT Libraries
{$(ALL_SRC_DIRS)}.c{$(MT_OBJ_DIR)}$(OBJEXT):
    $(CC) $(CLIBFLAGS) $(CFLAGSMT) -o$@ $<
#**********************************************************
# General *.prg --> *.obj COMPILE rule for STATIC MT Libraries
{$(ALL_SRC_DIRS)}.prg{$(MT_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSLIB) -o$(MT_OBJ_DIR)\ $**
    $(CC) $(CLIBFLAGS) $(CFLAGSMT) -o$@ $(MT_OBJ_DIR)\$&.c
#**********************************************************

#**********************************************************
# General *.c --> *.obj COMPILE rule for SHARED MT Libraries
{$(ALL_SRC_DIRS)}.c{$(MTDLL_OBJ_DIR)}$(OBJEXT):
    $(CC) $(CLIBFLAGSDLL) $(CFLAGSMT) -o$@ $<
#**********************************************************
# General *.prg --> *.obj COMPILE rule for SHARED MT Libraries
{$(ALL_SRC_DIRS)}.prg{$(MTDLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSLIB) -o$(MTDLL_OBJ_DIR)\ $**
    $(CC) $(CLIBFLAGSDLL) $(CFLAGSMT) -o$@ $(MTDLL_OBJ_DIR)\$&.c
#**********************************************************

#**********************************************************
# General *.c --> *.obj COMPILE rule for SHARED Libraries
{$(ALL_LIB_SRC_DIRS)}.c{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(CC) $(CLIBFLAGSDLL) -o$@ $<
#**********************************************************
# General *.prg --> *.obj COMPILE rule for SHARED Libraries
{$(ALL_LIB_SRC_DIRS)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSLIB) -o$(DLL_OBJ_DIR)\ $**
    $(CC) $(CLIBFLAGSDLL) -o$@ $(DLL_OBJ_DIR)\$&.c
#**********************************************************

#**********************************************************
# General *.c --> *.obj COMPILE rules for EXECUTABLES,
# which use Harbour SHARED Library compiled as DLL
{$(ALL_EXE_SRC_DIRS)}.c{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(CC) $(CEXEFLAGSDLL) -o$@ $<
#**********************************************************
# General *.prg --> *.obj COMPILE rules for EXECUTABLES,
# which use Harbour SHARED Library compiled as DLL
{$(ALL_EXE_SRC_DIRS)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSEXE) -o$(DLL_OBJ_DIR)\ $**
    $(CC) $(CEXEFLAGSDLL) -o$@ $(DLL_OBJ_DIR)\$&.c
#**********************************************************

#**********************************************************
# General BUILD rules (not used !)
#**********************************************************

# General Library BUILD rule
#{$(OBJ_DIR)}.obj{$(LIB_DIR)}.lib:
#    @if exist "$@" $(DEL) "$@" > NUL
#    $(MKLIB) "$@" $(ARFLAGS) @&&!
#+$(**: = &^
#+)
#!

#**********************************************************
#**********************************************************
#**********************************************************

#**********************************************************
# TARGET dependencies
#**********************************************************

all : $(HB_DEST_DIRS) $(HB_BUILD_TARGETS)

#**********************************************************
# Helper targets
#**********************************************************

BasicLibs : $(COMMON_LIB) $(PP_LIB) $(COMPILER_LIB)
BasicExes : $(HARBOUR_EXE)
StdLibs   : $(STANDARD_STATIC_HBLIBS)

#**********************************************************

$(HB_DEST_DIRS) $(HB_BIN_INSTALL) $(HB_LIB_INSTALL) $(HB_INC_INSTALL):
    @if not exist $@\nul mkdir $@

#**********************************************************
# LIBRARY Targets BUILD rules
#**********************************************************
$(HBMAINSTD_LIB): $(HBMAINSTD_LIB_OBJS)
    @if exist "$(HBMAINSTD_LIB)" $(DEL) "$(HBMAINSTD_LIB)" > NUL
    $(MKLIB) "$(HBMAINSTD_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(HBMAINWIN_LIB): $(HBMAINWIN_LIB_OBJS)
    @if exist "$(HBMAINWIN_LIB)" $(DEL) "$(HBMAINWIN_LIB)" > NUL
    $(MKLIB) "$(HBMAINWIN_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(COMMON_LIB)   : $(COMMON_LIB_OBJS)
    @if exist "$(COMMON_LIB)" $(DEL) "$(COMMON_LIB)" > NUL
    $(MKLIB) "$(COMMON_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(PP_LIB)       : $(PP_LIB_OBJS)
    @if exist "$(PP_LIB)" $(DEL) "$(PP_LIB)" > NUL
    $(MKLIB) "$(PP_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(COMPILER_LIB) : $(COMPILER_LIB_OBJS)
    @if exist "$(COMPILER_LIB)" $(DEL) "$(COMPILER_LIB)" > NUL
    $(MKLIB) "$(COMPILER_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(VM_LIB)       :: BasicExes
$(VM_LIB)       :: $(VM_LIB_OBJS)
    @if exist "$(VM_LIB)" $(DEL) "$(VM_LIB)" > NUL
    $(MKLIB) "$(VM_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(VMMT_LIB)     :: BasicExes
$(VMMT_LIB)     :: $(VMMT_LIB_OBJS)
    @if exist "$(VMMT_LIB)" $(DEL) "$(VMMT_LIB)" > NUL
    $(MKLIB) "$(VMMT_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(RTL_LIB)      :: BasicExes
$(RTL_LIB)      :: $(RTL_LIB_OBJS)
    @if exist "$(RTL_LIB)" $(DEL) "$(RTL_LIB)" > NUL
    $(MKLIB) "$(RTL_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(MACRO_LIB)    : $(MACRO_LIB_OBJS)
    @if exist "$(MACRO_LIB)" $(DEL) "$(MACRO_LIB)" > NUL
    $(MKLIB) "$(MACRO_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(DEBUG_LIB)    :: BasicExes
$(DEBUG_LIB)    :: $(DEBUG_LIB_OBJS)
    @if exist "$(DEBUG_LIB)" $(DEL) "$(DEBUG_LIB)" > NUL
    $(MKLIB) "$(DEBUG_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(LANG_LIB)     : $(LANG_LIB_OBJS)
    @if exist "$(LANG_LIB)" $(DEL) "$(LANG_LIB)" > NUL
    $(MKLIB) "$(LANG_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(CODEPAGE_LIB) : $(CODEPAGE_LIB_OBJS)
    @if exist "$(CODEPAGE_LIB)" $(DEL) "$(CODEPAGE_LIB)" > NUL
    $(MKLIB) "$(CODEPAGE_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(PCRE_LIB)     : $(PCRE_LIB_OBJS)
    @if exist "$(PCRE_LIB)" $(DEL) "$(PCRE_LIB)" > NUL
    $(MKLIB) "$(PCRE_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(HBZLIB_LIB)   : $(HBZLIB_LIB_OBJS)
    @if exist "$(HBZLIB_LIB)" $(DEL) "$(HBZLIB_LIB)" > NUL
    $(MKLIB) "$(HBZLIB_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(HBEXTERN_LIB) : $(HBEXTERN_LIB_OBJS)
    @if exist "$(HBEXTERN_LIB)" $(DEL) "$(HBEXTERN_LIB)" > NUL
    $(MKLIB) "$(HBEXTERN_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(RDD_LIB)      :: BasicExes
$(RDD_LIB)      :: $(RDD_LIB_OBJS)
    @if exist "$(RDD_LIB)" $(DEL) "$(RDD_LIB)" > NUL
    $(MKLIB) "$(RDD_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(NULSYS_LIB)   : $(NULSYS_LIB_OBJS)
    @if exist "$(NULSYS_LIB)" $(DEL) "$(NULSYS_LIB)" > NUL
    $(MKLIB) "$(NULSYS_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(DBFNTX_LIB)   :: BasicExes
$(DBFNTX_LIB)   :: $(DBFNTX_LIB_OBJS)
    @if exist "$(DBFNTX_LIB)" $(DEL) "$(DBFNTX_LIB)" > NUL
    $(MKLIB) "$(DBFNTX_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(DBFNSX_LIB)   :: BasicExes
$(DBFNSX_LIB)   :: $(DBFNSX_LIB_OBJS)
    @if exist "$(DBFNSX_LIB)" $(DEL) "$(DBFNSX_LIB)" > NUL
    $(MKLIB) "$(DBFNSX_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(DBFCDX_LIB)   : $(DBFCDX_LIB_OBJS)
    @if exist "$(DBFCDX_LIB)" $(DEL) "$(DBFCDX_LIB)" > NUL
    $(MKLIB) "$(DBFCDX_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(DBFFPT_LIB)   : $(DBFFPT_LIB_OBJS)
    @if exist "$(DBFFPT_LIB)" $(DEL) "$(DBFFPT_LIB)" > NUL
    $(MKLIB) "$(DBFFPT_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(HBSIX_LIB)    : $(HBSIX_LIB_OBJS)
    @if exist "$(HBSIX_LIB)" $(DEL) "$(HBSIX_LIB)" > NUL
    $(MKLIB) "$(HBSIX_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(HSX_LIB)      : $(HSX_LIB_OBJS)
    @if exist "$(HSX_LIB)" $(DEL) "$(HSX_LIB)" > NUL
    $(MKLIB) "$(HSX_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(USRRDD_LIB)   : $(USRRDD_LIB_OBJS)
    @if exist "$(USRRDD_LIB)" $(DEL) "$(USRRDD_LIB)" > NUL
    $(MKLIB) "$(USRRDD_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(HBUDDALL_LIB) : $(HBUDDALL_LIB_OBJS)
    @if exist "$(HBUDDALL_LIB)" $(DEL) "$(HBUDDALL_LIB)" > NUL
    $(MKLIB) "$(HBUDDALL_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(GTCGI_LIB)    : $(GTCGI_LIB_OBJS)
    @if exist "$(GTCGI_LIB)" $(DEL) "$(GTCGI_LIB)" > NUL
    $(MKLIB) "$(GTCGI_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(GTDOS_LIB)    : $(GTDOS_LIB_OBJS)
    @if exist "$(GTDOS_LIB)" $(DEL) "$(GTDOS_LIB)" > NUL
    $(MKLIB) "$(GTDOS_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(GTPCA_LIB)    : $(GTPCA_LIB_OBJS)
    @if exist "$(GTPCA_LIB)" $(DEL) "$(GTPCA_LIB)" > NUL
    $(MKLIB) "$(GTPCA_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(GTSTD_LIB)    : $(GTSTD_LIB_OBJS)
    @if exist "$(GTSTD_LIB)" $(DEL) "$(GTSTD_LIB)" > NUL
    $(MKLIB) "$(GTSTD_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(GTWIN_LIB)    : $(GTWIN_LIB_OBJS)
    @if exist "$(GTWIN_LIB)" $(DEL) "$(GTWIN_LIB)" > NUL
    $(MKLIB) "$(GTWIN_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(GTWVT_LIB)    : $(GTWVT_LIB_OBJS)
    @if exist "$(GTWVT_LIB)" $(DEL) "$(GTWVT_LIB)" > NUL
    $(MKLIB) "$(GTWVT_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(GTGUI_LIB)    : $(GTGUI_LIB_OBJS)
    @if exist "$(GTGUI_LIB)" $(DEL) "$(GTGUI_LIB)" > NUL
    $(MKLIB) "$(GTGUI_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************

#**********************************************************
# EXECUTABLE Targets
#**********************************************************

#**********************************************************
# HARBOUR build rule
#**********************************************************
$(HARBOUR_EXE) :: BasicLibs
$(HARBOUR_EXE) :: $(HARBOUR_EXE_OBJS)
    @if exist "$(HARBOUR_EXE)" $(DEL) "$(HARBOUR_EXE)" > NUL
    $(CC) @&&!
$(CFLAGS)
-e$(HARBOUR_EXE)
$(**: = ^
)
$(COMPILER_LIB)
$(COMMON_LIB)
$(PP_LIB)
!
#**********************************************************
# HBPP build rule
#**********************************************************
$(HBPP_EXE) :: $(COMMON_LIB)
$(HBPP_EXE) :: $(HBPP_EXE_OBJS)
    @if exist "$(HBPP_EXE)" $(DEL) "$(HBPP_EXE)" > NUL
    $(CC) @&&!
$(CFLAGS)
-e$(HBPP_EXE)
$(**: = ^
)
$(COMMON_LIB)
!
#**********************************************************
# HBRUN build rule
#**********************************************************
$(HBRUN_EXE)  :: BasicLibs BasicExes StdLibs
$(HBRUN_EXE)  :: $(HBRUN_EXE_OBJS)
    @if exist "$(HBRUN_EXE)" $(DEL) "$(HBRUN_EXE)" > NUL
    $(CC) @&&!
$(CFLAGS)
-e$(HBRUN_EXE)
$(**: = ^
)
$(STANDARD_STATIC_CLIBS)
$(STANDARD_STATIC_HBLIBS)
!
#**********************************************************
# HBTEST build rule
#**********************************************************
$(HBTEST_EXE) :: BasicLibs BasicExes StdLibs
$(HBTEST_EXE) :: $(HBTEST_EXE_OBJS)
    @if exist "$(HBTEST_EXE)" $(DEL) "$(HBTEST_EXE)" > NUL
    $(CC) @&&!
$(CFLAGS)
-e$(HBTEST_EXE)
$(**: = ^
)
$(STANDARD_STATIC_CLIBS)
$(STANDARD_STATIC_HBLIBS)
!
#**********************************************************
# HBI18N build rule
#**********************************************************
$(HBI18N_EXE) :: BasicLibs BasicExes StdLibs
$(HBI18N_EXE) :: $(HBI18N_EXE_OBJS)
    @if exist "$(HBI18N_EXE)" $(DEL) "$(HBI18N_EXE)" > NUL
    $(CC) @&&!
$(CFLAGS)
-e$(HBI18N_EXE)
$(**: = ^
)
$(STANDARD_STATIC_CLIBS)
$(MINIMAL_STATIC_HBLIBS)
!
#**********************************************************
# HBDOC build rule
#**********************************************************
$(HBDOC_EXE)  :: BasicLibs BasicExes StdLibs
$(HBDOC_EXE)  :: $(HBDOC_EXE_OBJS)
    @if exist "$(HBDOC_EXE)" $(DEL) "$(HBDOC_EXE)" > NUL
    $(CC) @&&!
$(CFLAGS)
-e$(HBDOC_EXE)
$(**: = ^
)
$(STANDARD_STATIC_CLIBS)
$(MINIMAL_STATIC_HBLIBS) $(HBDOC_LIBS)
!
#**********************************************************
# HBMK build rule
#**********************************************************
$(HBMK_EXE) :: BasicLibs BasicExes StdLibs
$(HBMK_EXE) :: $(HBMK_EXE_OBJS)
    @if exist "$(HBMK_EXE)" $(DEL) "$(HBMK_EXE)" > NUL
    $(CC) @&&!
$(CFLAGS)
-e$(HBMK_EXE)
$(**: = ^
)
$(STANDARD_STATIC_CLIBS)
$(MINIMAL_STATIC_HBLIBS)
$(COMPILER_LIB)
$(PP_LIB)
!
#**********************************************************

#**********************************************************
# DLL Targets
#**********************************************************
$(HARBOUR_DLL) :: BasicLibs BasicExes
$(HARBOUR_DLL) :: $(DLL_OBJS)
    $(LINKER) $(LDFLAGSDLL) @&&!
c0d32.obj $**, $@,, cw32mt$(RTLIBSUFFIX).lib import32.lib
!
#**********************************************************
$(HARBOURMT_DLL) :: BasicLibs BasicExes
$(HARBOURMT_DLL) :: $(MTDLL_OBJS)
    $(LINKER) $(LDFLAGSDLL) @&&!
c0d32.obj $**, $@,, cw32mt$(RTLIBSUFFIX).lib import32.lib
!
#**********************************************************
# DLL EXECUTABLE Targets
#**********************************************************
$(HBTESTDLL_EXE) :: BasicLibs BasicExes
$(HBTESTDLL_EXE) :: $(DLL_OBJ_DIR)\mainstd$(OBJEXT) $(HBTEST_EXE_OBJS:$(OBJ_DIR)=$(DLL_OBJ_DIR))
    $(LINKER) $(LDFLAGS) @&&!
c0x32.obj $**, $@,,$(HARBOUR_DLL:$(DLLEXT)=$(LIBEXT)) cw32mt$(RTLIBSUFFIX).lib import32.lib $(COMMON_LIB)
!
#**********************************************************
$(HBRUNDLL_EXE) :: BasicLibs BasicExes
$(HBRUNDLL_EXE) :: $(DLL_OBJ_DIR)\mainstd$(OBJEXT) $(HBRUN_EXE_OBJS:$(OBJ_DIR)=$(DLL_OBJ_DIR))
    $(LINKER) $(LDFLAGS) @&&!
c0x32.obj $**, $@,,$(HARBOUR_DLL:$(DLLEXT)=$(LIBEXT)) cw32mt$(RTLIBSUFFIX).lib import32.lib $(COMPILER_LIB) $(PP_LIB) $(COMMON_LIB)
!
#----------------------------------------------------------
$(DLL_OBJ_DIR)\mainstd$(OBJEXT) : $(VM_DIR)\mainstd.c
    $(CC) $(CEXEFLAGSDLL) -o$@ $**
#**********************************************************

#**********************************************************
# EXTRA Object's DEPENDENCIES
#**********************************************************

#**********************************************************

# Generated by an intermediate utility hbpp.exe
# built at the initial phase of build process
$(OBJ_DIR)\pptable.obj     : $(OBJ_DIR)\pptable.c
$(DLL_OBJ_DIR)\pptable.obj : $(DLL_OBJ_DIR)\pptable.c

$(OBJ_DIR)\pptable.c     : $(INCLUDE_DIR)\hbstdgen.ch $(INCLUDE_DIR)\std.ch ChangeLog $(PP_DIR)\ppcore.c $(PP_DIR)\hbpp.c
    @if exist "$(OBJ_DIR)\pptable.c" $(DEL) "$(OBJ_DIR)\pptable.c" > nul
    $(HBPP) $(INCLUDE_DIR)/hbstdgen.ch -o$(OBJ_DIR)/pptable.c -q -cChangeLog -v$(INCLUDE_DIR)/hbverbld.h

$(DLL_OBJ_DIR)\pptable.c : $(INCLUDE_DIR)\hbstdgen.ch $(INCLUDE_DIR)\std.ch ChangeLog $(PP_DIR)\ppcore.c $(PP_DIR)\hbpp.c
    @if exist "$(DLL_OBJ_DIR)\pptable.c" $(DEL) "$(DLL_OBJ_DIR)\pptable.c" > nul
    $(HBPP) $(INCLUDE_DIR)/hbstdgen.ch -o$(DLL_OBJ_DIR)/pptable.c -q -cChangeLog -v$(INCLUDE_DIR)/hbverbld.h

#**********************************************************

!if "$(HB_REBUILD_PARSER)" == "yes"

$(OBJ_DIR)\harboury.c : $(COMPILER_DIR)\harbour.y
    bison --no-line -d $** -o$@

$(OBJ_DIR)\macroy.c : $(MACRO_DIR)\macro.y
    bison --no-line -d $** -o$@

$(DLL_OBJ_DIR)\harboury.c : $(COMPILER_DIR)\harbour.y
    bison --no-line -d $** -o$@

$(DLL_OBJ_DIR)\macroy.c : $(MACRO_DIR)\macro.y
    bison --no-line -d $** -o$@

!else

$(OBJ_DIR)\harboury.c : $(COMPILER_DIR)\harbour.yyc
    copy /A $** $@
    copy /A $(**:.yyc=.yyh) $(@:.c=.h)

$(OBJ_DIR)\macroy.c : $(MACRO_DIR)\macro.yyc
    copy /A $** $@
    copy /A $(**:.yyc=.yyh) $(@:.c=.h)

$(DLL_OBJ_DIR)\harboury.c : $(COMPILER_DIR)\harbour.yyc
    copy /A $** $@
    copy /A $(**:.yyc=.yyh) $(@:.c=.h)

$(DLL_OBJ_DIR)\macroy.c : $(MACRO_DIR)\macro.yyc
    copy /A $** $@
    copy /A $(**:.yyc=.yyh) $(@:.c=.h)

!endif

$(OBJ_DIR)\harboury.obj : $(OBJ_DIR)\harboury.c
$(OBJ_DIR)\macroy.obj   : $(OBJ_DIR)\macroy.c

$(DLL_OBJ_DIR)\harboury.obj : $(DLL_OBJ_DIR)\harboury.c
$(DLL_OBJ_DIR)\macroy.obj   : $(DLL_OBJ_DIR)\macroy.c

#**********************************************************
#**********************************************************
#**********************************************************

#**********************************************************
# CLEAN rules
#**********************************************************

clean: doClean
Clean: doClean
CLEAN: doClean

doClean:
    -if exist $(BIN_DIR)\*.exe          $(DEL) $(BIN_DIR)\*.exe          > nul
    -if exist $(BIN_DIR)\*.tds          $(DEL) $(BIN_DIR)\*.tds          > nul
    -if exist $(BIN_DIR)\*.tr?          $(DEL) $(BIN_DIR)\*.tr?          > nul
    -if exist $(BIN_DIR)\*.map          $(DEL) $(BIN_DIR)\*.map          > nul
    -if exist $(BIN_DIR)\*.dll          $(DEL) $(BIN_DIR)\*.dll          > nul
    -if exist $(BIN_DIR)\*.lib          $(DEL) $(BIN_DIR)\*.lib          > nul
    -if exist $(LIB_DIR)\*.lib          $(DEL) $(LIB_DIR)\*.lib          > nul
    -if exist $(LIB_DIR)\*.bak          $(DEL) $(LIB_DIR)\*.bak          > nul
    -if exist $(OBJ_DIR)\*.obj          $(DEL) $(OBJ_DIR)\*.obj          > nul
    -if exist $(OBJ_DIR)\*.c            $(DEL) $(OBJ_DIR)\*.c            > nul
    -if exist $(OBJ_DIR)\*.h            $(DEL) $(OBJ_DIR)\*.h            > nul
    -if exist $(DLL_OBJ_DIR)\*.obj      $(DEL) $(DLL_OBJ_DIR)\*.obj      > nul
    -if exist $(DLL_OBJ_DIR)\*.c        $(DEL) $(DLL_OBJ_DIR)\*.c        > nul
    -if exist $(DLL_OBJ_DIR)\*.h        $(DEL) $(DLL_OBJ_DIR)\*.h        > nul
    -if exist $(MT_OBJ_DIR)\*.obj       $(DEL) $(MT_OBJ_DIR)\*.obj       > nul
    -if exist $(MT_OBJ_DIR)\*.c         $(DEL) $(MT_OBJ_DIR)\*.c         > nul
    -if exist $(MT_OBJ_DIR)\*.h         $(DEL) $(MT_OBJ_DIR)\*.h         > nul
    -if exist $(MTDLL_OBJ_DIR)\*.obj    $(DEL) $(MTDLL_OBJ_DIR)\*.obj    > nul
    -if exist $(MTDLL_OBJ_DIR)\*.c      $(DEL) $(MTDLL_OBJ_DIR)\*.c      > nul
    -if exist $(MTDLL_OBJ_DIR)\*.h      $(DEL) $(MTDLL_OBJ_DIR)\*.h      > nul
    -if exist $(INCLUDE_DIR)\hbverbld.h $(DEL) $(INCLUDE_DIR)\hbverbld.h > nul
    -if exist inst_$(HB_CC_NAME).log    $(DEL) inst_$(HB_CC_NAME).log    > nul
    -if exist bin\*.exe                 $(DEL) bin\*.exe                 > nul
    -if exist bin\*.dll                 $(DEL) bin\*.dll                 > nul
    -if exist lib\*.lib                 $(DEL) lib\*.lib                 > nul

#**********************************************************
# INSTALL rules
#**********************************************************

install : doInstall
Install : doInstall
INSTALL : doInstall

doInstall: $(HB_BIN_INSTALL) $(HB_LIB_INSTALL) $(HB_INC_INSTALL)
    -if exist $(HB_BIN_INSTALL)\nul if exist $(BIN_DIR)\*.exe copy /B $(BIN_DIR)\*.exe $(HB_BIN_INSTALL) >  inst_$(HB_CC_NAME).log
    -if exist $(HB_BIN_INSTALL)\nul if exist $(BIN_DIR)\*.dll copy /B $(BIN_DIR)\*.dll $(HB_BIN_INSTALL) >> inst_$(HB_CC_NAME).log
    -if exist $(HB_LIB_INSTALL)\nul if exist $(BIN_DIR)\*.lib copy /B $(BIN_DIR)\*.lib $(HB_LIB_INSTALL) >> inst_$(HB_CC_NAME).log
    -if exist $(HB_LIB_INSTALL)\nul if exist $(LIB_DIR)\*.lib copy /B $(LIB_DIR)\*.lib $(HB_LIB_INSTALL) >> inst_$(HB_CC_NAME).log
!if "$(HB_INSTALL_PREFIX)" != "."
    -if exist $(HB_BIN_INSTALL)\nul copy /B bin\hbmk.bat         $(HB_BIN_INSTALL) >> inst_$(HB_CC_NAME).log
    -if exist $(HB_INC_INSTALL)\nul copy /B $(INCLUDE_DIR)\*.api $(HB_INC_INSTALL) >> inst_$(HB_CC_NAME).log
    -if exist $(HB_INC_INSTALL)\nul copy /B $(INCLUDE_DIR)\*.ch  $(HB_INC_INSTALL) >> inst_$(HB_CC_NAME).log
    -if exist $(HB_INC_INSTALL)\nul copy /B $(INCLUDE_DIR)\*.h   $(HB_INC_INSTALL) >> inst_$(HB_CC_NAME).log
!endif

#**********************************************************
