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
#       C_USR             - Extra C compiler options for libraries and for executables
#       CLIBFLAGSDLL      - Extra C compiler options for the shared libraries
#
#       L_USR             - Extra linker options for the static libraries
#       LDFLAGSDLL        - Extra linker options for the shared libraries
#
#       PRG_USR           - Extra Harbour compiler options
#       HARBOURFLAGSDLL   - Extra Harbour compiler options for shared libraries
#
#       HB_GT_DEFAULT     - The default GT driver, Choose between:
#                           gtwin (default),gtcgi,gtwvt,gtstd
#       HB_GT_LIB         - To override the default GT driver
#                           (search for HB_GT_LIBS for a list of values)
#       HB_BUILD_ST       - If set to yes builds harbour in SingleThread mode
#       HB_BUILD_DLL      - If set to yes enables building harbour VM+RTL
#                           dll in addition to normal static build
#       HB_BUILD_DEBUG    - If set to yes causes to compile with debug info
#       HB_BUILD_VERBOSE  - enables echoing commands being executed
#       HB_REBUILD_PARSER - If set to yes force preprocessing new rules by
#                           bison (you must use bison 2.3 or later)
#       HB_INSTALL_PREFIX - Path to installation directory into which
#                           Harbour will be installed when the command
#                           "make_bc.bat install" is lauched. Defaults
#                           to current directory
#       HB_BCCDLL_DYNRT   - If set to -tWR causes that harbour-bc.dll
#                           will use dynamic runtime library (recommended)

#**********************************************************

#.KEEP
.AUTODEPEND
.SUFFIXES:

#**********************************************************

HB_ARCHITECTURE = w32

#**********************************************************

# BORLAND has ST mode as default
!if "$(HB_BUILD_ST)" == ""
    HB_BUILD_ST = yes
!endif

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

# Some definitions cannot be kept in Common.mak
# due to serious limitations of Microsoft Nmake

DLL_OBJS = $(TMP_DLL_OBJS:$(OBJ_DIR)=$(DLL_OBJ_DIR))

VMMT_LIB_OBJS = $(VM_LIB_OBJS:$(OBJ_DIR)=$(MT_OBJ_DIR))

#**********************************************************
# C compiler, Harbour compiler and Linker flags.
#**********************************************************

# Main "Include" directory
INCLUDE_DIR = include

#**********************************************************

# C Compiler Flags
CFLAGS = -I$(INCLUDE_DIR) -I$(OBJ_DIR) $(C_USR)
CFLAGSMT = -tWM -DHB_MT_VM $(CFLAGSMT)

#-----------
!ifndef BCC_NOOPTIM
    CFLAGS = -O2 $(CFLAGS)
!endif
#-----------
!if "$(HB_BUILD_DEBUG)" == "yes"
    CFLAGS = -y -v $(CFLAGS)
!endif
#-----------
!if "$(HB_BCCDLL_DYNRT)" == "-tWR"
    HB_BCCDLL_DYNRT=$(HB_BCCDLL_DYNRT)
    RTLIBSUFFIX = i
!endif
#-----------
!if "$(HB_BUILD_ST)" != "yes"
    CFLAGS = $(CFLAGS) $(CFLAGSMT)
!else
    HB_BUILD_TARGETS = $(HB_BUILD_TARGETS) $(VMMT_LIB)
!endif
#-----------
!if "$(HB_GT_DEFAULT)" != ""
    CFLAGS = -DHB_GT_DEFAULT=$(HB_GT_DEFAULT:gt=) $(CFLAGS)
!endif
#-----------
!if "$(HB_GT_LIB)" != ""
    CFLAGS = -DHB_GT_LIB=$(HB_GT_LIB:gt=) $(CFLAGS)
!endif
#-----------

#**********************************************************

CLIBFLAGS      = -c -q -d -w -w-sig- $(CFLAGS)
CLIBFLAGSxxx   = $(HB_BCCDLL_DYNRT) $(CLIBFLAGS: -tWM= )
CLIBFLAGSDLL   = -tWM $(CLIBFLAGSxxx) $(CLIBFLAGSDLL) -DHB_DYNLIB
CEXEFLAGSDLL   = -tWM $(CLIBFLAGSxxx) $(CEXEFLAGSDLL)

#**********************************************************

# Harbour Compiler Flags
HBFLAGSCMN     = -i$(INCLUDE_DIR) -q0 -w3 -es2 -km $(PRG_USR)
HARBOURFLAGS   = -n $(HBFLAGSCMN)
HARBOURFLAGSDLL= -n1 $(HBFLAGSCMN) $(HARBOURFLAGSDLL)

#**********************************************************

# Linker Flags
LDFLAGS        = -Gn -C -ap -Tpe -L$(LIB_DIR) -L$(BIN_DIR) $(L_USR)
LDFLAGSDLL     = -Gn -C -aa -Tpd -Gi -L$(LIB_DIR) $(LDFLAGSDLL)
!if "$(HB_BUILD_DEBUG)" == "yes"
    LDFLAGS = -v $(LDFLAGS)
    LDFLAGSDLL = -v $(LDFLAGSDLL)
!endif

#**********************************************************

!if "$(HB_BUILD_ST)" != "yes"
    STANDARD_STATIC_CLIBS = cw32mt$(RTLIBSUFFIX).lib
!endif

# This is needed, otherwise the libs may overflow
# when debug info is requested with -v -y
ARFLAGS = /P32 $(A_USR)

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
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\  $**
    $(CC) $(CLIBFLAGS) -o$@ $(OBJ_DIR)\$&.c
#**********************************************************

#**********************************************************
# General *.c --> *.obj COMPILE rule for STATIC MT Libraries
{$(ALL_SRC_DIRS)}.c{$(MT_OBJ_DIR)}$(OBJEXT):
    $(CC) $(CLIBFLAGS) $(CFLAGSMT) -o$@ $<
#**********************************************************
# General *.prg --> *.obj COMPILE rule for STATIC MT Libraries
{$(ALL_SRC_DIRS)}.prg{$(MT_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(MT_OBJ_DIR)\  $**
    $(CC) $(CLIBFLAGS) $(CFLAGSMT) -o$@ $(MT_OBJ_DIR)\$&.c
#**********************************************************

#**********************************************************
# General *.c --> *.obj COMPILE rule for SHARED Libraries
{$(ALL_LIB_SRC_DIRS)}.c{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(CC) $(CLIBFLAGSDLL) -o$@ $<
#**********************************************************
# General *.prg --> *.obj COMPILE rule for SHARED Libraries
{$(ALL_LIB_SRC_DIRS)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\  $**
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
    $(HB) $(HARBOURFLAGS) -o$(DLL_OBJ_DIR)\  $**
    $(CC) $(CEXEFLAGSDLL) -o$@ $(DLL_OBJ_DIR)\$&.c
#**********************************************************

#**********************************************************
# General BUILD rules (not used !)
#**********************************************************

# General Library BUILD rule
#{$(OBJ_DIR)}.obj{$(LIB_DIR)}.lib:
#    IF EXIST "$@" $(DEL) "$@" > NUL
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
    !if not exist $@\nul mkdir $@

#**********************************************************
# LIBRARY Targets BUILD rules
#**********************************************************
$(COMMON_LIB)   : $(COMMON_LIB_OBJS)
    IF EXIST "$(COMMON_LIB)" $(DEL) "$(COMMON_LIB)" > NUL
    $(MKLIB) "$(COMMON_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(PP_LIB)       : $(PP_LIB_OBJS)
    IF EXIST "$(PP_LIB)" $(DEL) "$(PP_LIB)" > NUL
    $(MKLIB) "$(PP_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(COMPILER_LIB) : $(COMPILER_LIB_OBJS)
    IF EXIST "$(COMPILER_LIB)" $(DEL) "$(COMPILER_LIB)" > NUL
    $(MKLIB) "$(COMPILER_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(VM_LIB)       :: BasicExes
$(VM_LIB)       :: $(VM_LIB_OBJS)
    IF EXIST "$(VM_LIB)" $(DEL) "$(VM_LIB)" > NUL
    $(MKLIB) "$(VM_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(VMMT_LIB)     :: BasicExes
$(VMMT_LIB)     :: $(VMMT_LIB_OBJS)
    IF EXIST "$(VMMT_LIB)" $(DEL) "$(VMMT_LIB)" > NUL
    $(MKLIB) "$(VMMT_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(RTL_LIB)      :: BasicExes
$(RTL_LIB)      :: $(RTL_LIB_OBJS)
    IF EXIST "$(RTL_LIB)" $(DEL) "$(RTL_LIB)" > NUL
    $(MKLIB) "$(RTL_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(MACRO_LIB)    : $(MACRO_LIB_OBJS)
    IF EXIST "$(MACRO_LIB)" $(DEL) "$(MACRO_LIB)" > NUL
    $(MKLIB) "$(MACRO_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(DEBUG_LIB)    :: BasicExes
$(DEBUG_LIB)    :: $(DEBUG_LIB_OBJS)
    IF EXIST "$(DEBUG_LIB)" $(DEL) "$(DEBUG_LIB)" > NUL
    $(MKLIB) "$(DEBUG_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(LANG_LIB)     : $(LANG_LIB_OBJS)
    IF EXIST "$(LANG_LIB)" $(DEL) "$(LANG_LIB)" > NUL
    $(MKLIB) "$(LANG_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(CODEPAGE_LIB) : $(CODEPAGE_LIB_OBJS)
    IF EXIST "$(CODEPAGE_LIB)" $(DEL) "$(CODEPAGE_LIB)" > NUL
    $(MKLIB) "$(CODEPAGE_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(PCRE_LIB)     : $(PCRE_LIB_OBJS)
    IF EXIST "$(PCRE_LIB)" $(DEL) "$(PCRE_LIB)" > NUL
    $(MKLIB) "$(PCRE_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(HBZLIB_LIB)   : $(HBZLIB_LIB_OBJS)
    IF EXIST "$(HBZLIB_LIB)" $(DEL) "$(HBZLIB_LIB)" > NUL
    $(MKLIB) "$(HBZLIB_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(HBEXTERN_LIB) : $(HBEXTERN_LIB_OBJS)
    IF EXIST "$(HBEXTERN_LIB)" $(DEL) "$(HBEXTERN_LIB)" > NUL
    $(MKLIB) "$(HBEXTERN_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(RDD_LIB)      :: BasicExes
$(RDD_LIB)      :: $(RDD_LIB_OBJS)
    IF EXIST "$(RDD_LIB)" $(DEL) "$(RDD_LIB)" > NUL
    $(MKLIB) "$(RDD_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(NULSYS_LIB)   : $(NULSYS_LIB_OBJS)
    IF EXIST "$(NULSYS_LIB)" $(DEL) "$(NULSYS_LIB)" > NUL
    $(MKLIB) "$(NULSYS_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(DBFNTX_LIB)   :: BasicExes
$(DBFNTX_LIB)   :: $(DBFNTX_LIB_OBJS)
    IF EXIST "$(DBFNTX_LIB)" $(DEL) "$(DBFNTX_LIB)" > NUL
    $(MKLIB) "$(DBFNTX_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(DBFCDX_LIB)   : $(DBFCDX_LIB_OBJS)
    IF EXIST "$(DBFCDX_LIB)" $(DEL) "$(DBFCDX_LIB)" > NUL
    $(MKLIB) "$(DBFCDX_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(DBFFPT_LIB)   : $(DBFFPT_LIB_OBJS)
    IF EXIST "$(DBFFPT_LIB)" $(DEL) "$(DBFFPT_LIB)" > NUL
    $(MKLIB) "$(DBFFPT_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(HBSIX_LIB)    : $(HBSIX_LIB_OBJS)
    IF EXIST "$(HBSIX_LIB)" $(DEL) "$(HBSIX_LIB)" > NUL
    $(MKLIB) "$(HBSIX_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(HSX_LIB)      : $(HSX_LIB_OBJS)
    IF EXIST "$(HSX_LIB)" $(DEL) "$(HSX_LIB)" > NUL
    $(MKLIB) "$(HSX_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(USRRDD_LIB)   : $(USRRDD_LIB_OBJS)
    IF EXIST "$(USRRDD_LIB)" $(DEL) "$(USRRDD_LIB)" > NUL
    $(MKLIB) "$(USRRDD_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(GTCGI_LIB)    : $(GTCGI_LIB_OBJS)
    IF EXIST "$(GTCGI_LIB)" $(DEL) "$(GTCGI_LIB)" > NUL
    $(MKLIB) "$(GTCGI_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(GTDOS_LIB)    : $(GTDOS_LIB_OBJS)
    IF EXIST "$(GTDOS_LIB)" $(DEL) "$(GTDOS_LIB)" > NUL
    $(MKLIB) "$(GTDOS_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(GTPCA_LIB)    : $(GTPCA_LIB_OBJS)
    IF EXIST "$(GTPCA_LIB)" $(DEL) "$(GTPCA_LIB)" > NUL
    $(MKLIB) "$(GTPCA_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(GTSTD_LIB)    : $(GTSTD_LIB_OBJS)
    IF EXIST "$(GTSTD_LIB)" $(DEL) "$(GTSTD_LIB)" > NUL
    $(MKLIB) "$(GTSTD_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(GTWIN_LIB)    : $(GTWIN_LIB_OBJS)
    IF EXIST "$(GTWIN_LIB)" $(DEL) "$(GTWIN_LIB)" > NUL
    $(MKLIB) "$(GTWIN_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(GTWVT_LIB)    : $(GTWVT_LIB_OBJS)
    IF EXIST "$(GTWVT_LIB)" $(DEL) "$(GTWVT_LIB)" > NUL
    $(MKLIB) "$(GTWVT_LIB)" $(ARFLAGS) @&&!
+$(**: = &^
+)
!
#**********************************************************
$(GTGUI_LIB)    : $(GTGUI_LIB_OBJS)
    IF EXIST "$(GTGUI_LIB)" $(DEL) "$(GTGUI_LIB)" > NUL
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
    IF EXIST "$(HARBOUR_EXE)" $(DEL) "$(HARBOUR_EXE)" > NUL
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
    IF EXIST "$(HBPP_EXE)" $(DEL) "$(HBPP_EXE)" > NUL
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
    IF EXIST "$(HBRUN_EXE)" $(DEL) "$(HBRUN_EXE)" > NUL
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
    IF EXIST "$(HBTEST_EXE)" $(DEL) "$(HBTEST_EXE)" > NUL
    $(CC) @&&!
$(CFLAGS)
-e$(HBTEST_EXE)
$(**: = ^
)
$(STANDARD_STATIC_CLIBS)
$(STANDARD_STATIC_HBLIBS)
!
#**********************************************************
# HBDOC build rule
#**********************************************************
$(HBDOC_EXE)  :: BasicLibs BasicExes StdLibs
$(HBDOC_EXE)  :: $(HBDOC_EXE_OBJS)
    IF EXIST "$(HBDOC_EXE)" $(DEL) "$(HBDOC_EXE)" > NUL
    $(CC) @&&!
$(CFLAGS)
-e$(HBDOC_EXE)
$(**: = ^
)
$(STANDARD_STATIC_CLIBS)
$(MINIMAL_STATIC_HBLIBS) $(HBDOC_LIBS)
!
#**********************************************************
# HBMAKE build rule
#**********************************************************
$(HBMAKE_EXE) :: BasicLibs BasicExes StdLibs
$(HBMAKE_EXE) :: $(HBMAKE_EXE_OBJS)
    IF EXIST "$(HBMAKE_EXE)" $(DEL) "$(HBMAKE_EXE)" > NUL
    $(CC) @&&!
$(CFLAGS)
-e$(HBMAKE_EXE)
$(**: = ^
)
$(STANDARD_STATIC_CLIBS)
$(MINIMAL_STATIC_HBLIBS)
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
# DLL EXECUTABLE Targets
#**********************************************************
$(HBTESTDLL_EXE) :: BasicLibs BasicExes
$(HBTESTDLL_EXE) :: $(DLL_OBJ_DIR)\mainstd$(OBJEXT) $(HBTEST_EXE_OBJS:$(OBJ_DIR)=$(DLL_OBJ_DIR))
    $(LINKER) $(LDFLAGS) @&&!
c0x32.obj $**, $@,,$(HARBOUR_DLL:$(DLLEXT)=$(LIBEXT)) cw32mt$(RTLIBSUFFIX).lib import32.lib
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
    IF EXIST "$(OBJ_DIR)\pptable.c" $(DEL) "$(OBJ_DIR)\pptable.c" > nul
    $(HBPP) $(INCLUDE_DIR)/hbstdgen.ch -o$(OBJ_DIR)/pptable.c -q -cChangeLog -v$(INCLUDE_DIR)/hbverbld.h

$(DLL_OBJ_DIR)\pptable.c : $(INCLUDE_DIR)\hbstdgen.ch $(INCLUDE_DIR)\std.ch ChangeLog $(PP_DIR)\ppcore.c $(PP_DIR)\hbpp.c
    IF EXIST "$(DLL_OBJ_DIR)\pptable.c" $(DEL) "$(DLL_OBJ_DIR)\pptable.c" > nul
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
    -if exist $(MT_OBJ_DIR)\*.obj       $(DEL) $(MT_OBJ_DIR)\*.obj       > nul
    -if exist $(MT_OBJ_DIR)\*.c         $(DEL) $(MT_OBJ_DIR)\*.c         > nul
    -if exist $(MT_OBJ_DIR)\*.h         $(DEL) $(MT_OBJ_DIR)\*.h         > nul
    -if exist $(DLL_OBJ_DIR)\*.obj      $(DEL) $(DLL_OBJ_DIR)\*.obj      > nul
    -if exist $(DLL_OBJ_DIR)\*.c        $(DEL) $(DLL_OBJ_DIR)\*.c        > nul
    -if exist $(DLL_OBJ_DIR)\*.h        $(DEL) $(DLL_OBJ_DIR)\*.h        > nul
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
    -if exist $(HB_BIN_INSTALL)\nul if exist $(BIN_DIR)\*.exe   copy /B $(BIN_DIR)\*.exe $(HB_BIN_INSTALL) >  inst_$(HB_CC_NAME).log
    -if exist $(HB_BIN_INSTALL)\nul if exist $(BIN_DIR)\*.dll   copy /B $(BIN_DIR)\*.dll $(HB_BIN_INSTALL) >> inst_$(HB_CC_NAME).log
    -if exist $(HB_LIB_INSTALL)\nul if exist $(BIN_DIR)\*.lib   copy /B $(BIN_DIR)\*.lib $(HB_LIB_INSTALL) >> inst_$(HB_CC_NAME).log
    -if exist $(HB_LIB_INSTALL)\nul if exist $(LIB_DIR)\*.lib   copy /B $(LIB_DIR)\*.lib $(HB_LIB_INSTALL) >> inst_$(HB_CC_NAME).log
!if "$(HB_INSTALL_PREFIX)" != "."
    -if exist $(HB_INC_INSTALL)\nul   copy /A $(INCLUDE_DIR)\*.api $(HB_INC_INSTALL) >> inst_$(HB_CC_NAME).log
    -if exist $(HB_INC_INSTALL)\nul   copy /A $(INCLUDE_DIR)\*.ch  $(HB_INC_INSTALL) >> inst_$(HB_CC_NAME).log
    -if exist $(HB_INC_INSTALL)\nul   copy /A $(INCLUDE_DIR)\*.h   $(HB_INC_INSTALL) >> inst_$(HB_CC_NAME).log
!endif

#**********************************************************
