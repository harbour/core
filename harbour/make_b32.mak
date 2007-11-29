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
#       C_USR             - Extra C compiler options for libraries and for
#                           executables (GNU make compatible envvar)
#       CLIBFLAGS         - Extra C compiler options for the static libraries
#       CLIBFLAGSDLL      - Extra C compiler options for the shared libraries
#
#       LDFLAGS           - Extra linker options for the static libraries
#       LDFLAGSDLL        - Extra linker options for the shared libraries
#
#       HARBOURFLAGS      - Extra Harbour compiler options for static libs/exes
#       HARBOURFLAGSDLL   - Extra Harbour compiler options for shared libraries
#       PRG_USR           - Extra Harbour compiler options
#                           (GNU make compatible envvar)
#       HB_GT_DEFAULT     - The default GT driver, Choose between:
#                           gtstd (default),gtcgi,gtwin,gtwvt
#       HB_GT_LIB         - To override the default GT driver
#                           (search for HB_GT_LIBS for a list of values)
#       HB_BUILD_DLL      - If set to yes enables building harbour VM+RTL
#                           dll in addition to normal static build
#       HB_BUILD_DEBUG    - If set to yes causes to compile with debug info
#       HB_BUILD_VERBOSE  - enables echoing commands being executed
#       HB_REBUILD_PARSER - If set to yes force preprocessing new rules by
#                           bison (you must use bison 2.3 or later)
#       BCCDLL_WITH_DYNRT - If set to -tWR causes that harbour-bc.dll
#                           will use dynamic runtime library (recommended)
#       HB_INSTALL_PREFIX - Path to instalation directory into which
#                           Harbour will be installed when the command
#                           "make_bc.bat install" is lauched. Defaults
#                           to current directory
#
#       HB_DOC_PDF        - Turns on the .PDF file support in the HBDOC utility.
#                           Note that this will require the pdflib contrib.
#**********************************************************

#.KEEP
.AUTODEPEND
.SUFFIXES:

!if "$(HB_GT_LIB)" == ""
HB_GT_LIB = gtwin
!endif

#**********************************************************

CC     = bcc32.exe
LINKER = ilink32.exe
MKLIB  = tlib.exe

#**********************************************************

# Include Common Object list files
# shared between Msvc and Borland

!include common.mak

#**********************************************************

.SUFFIXES: $(EXEEXT) $(LIBEXT) $(OBJEXT) .prg .c .l .y

#**********************************************************

# Some definitions cannot be kept in Common.mak
# due to serious limitations of Microsoft Nmake

DLL_OBJS = $(TMP_DLL_OBJS:$(OBJ_DIR)=$(DLL_OBJ_DIR))

#**********************************************************
# C compiler, Harbour compiler and Linker flags.
#**********************************************************

# Main "Include" directory
INCLUDE_DIR    = include

# C Compiler Flags
CFLAGS         = -I$(INCLUDE_DIR) $(C_USR) $(CFLAGS) -I$(OBJ_DIR)
#-----------
!ifndef BCC_NOOPTIM
    CFLAGS     = -O2 $(CFLAGS)
!endif
#-----------
!if "$(HB_BUILD_DEBUG)" == "yes"
    CFLAGS     = -y -v $(CFLAGS)
!endif
#-----------
!if "$(BCCDLL_WITH_DYNRT)" == "-tWR"
    BCCDLL_WITH_DYNRT=$(BCCDLL_WITH_DYNRT) -DHB_NO_BCC_MAX_OPENFILES_HACK
    RTLIBSUFFIX = i
!endif
#-----------
!if "$(HB_GT_DEFAULT)" != ""
    CFLAGS     = -DHB_GT_DEFAULT=$(HB_GT_DEFAULT:gt=) $(CFLAGS)
!endif
#-----------
!if "$(HB_GT_LIB)" != ""
    CFLAGS     = -DHB_GT_LIB=$(HB_GT_LIB:gt=) $(CFLAGS)
!endif
#-----------
CLIBFLAGS      = -c -q $(CFLAGS) $(CLIBFLAGS)
CLIBFLAGSDLL   = -D__EXPORT__ -tWM $(BCCDLL_WITH_DYNRT) $(CLIBFLAGS) $(CLIBFLAGSDLL)
CEXEFLAGSDLL   = -tWM $(BCCDLL_WITH_DYNRT) $(CLIBFLAGS) $(CEXEFLAGSDLL)

# Harbour Compiler Flags
HBFLAGSCMN     = -i$(INCLUDE_DIR) -q0 -w2 -es2 -gc0 -kM $(PRG_USR)
!ifdef HB_DOC_PDF
    HBFLAGSCMN = $(HBFLAGSCMN) -dPDF
!endif
HARBOURFLAGS   = -n $(HBFLAGSCMN) $(HARBOURFLAGS)
HARBOURFLAGSDLL= -D__EXPORT__ -n1 $(HBFLAGSCMN) $(HARBOURFLAGSDLL)

# Linker Flags
LDFLAGS        = -ap -Tpe -Gn -C -L$(LIB_DIR) -L$(BIN_DIR) $(LDFLAGS)
LDFLAGSDLL     = -aa -Gn -C -Tpd -Gi -L$(LIB_DIR) $(LDFLAGSDLL)
!if "$(HB_BUILD_DEBUG)" == "yes"
    LDFLAGS    = -v $(LDFLAGS)
    LDFLAGSDLL = -v $(LDFLAGSDLL)
!endif

#**********************************************************

# This is needed, otherwise the libs may overflow
# when debug info is requested with -v -y
ARFLAGS = /P32

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
# HBPPGEN build rule
#**********************************************************
$(HBPPGEN_EXE) :: $(COMMON_LIB)
$(HBPPGEN_EXE) :: $(HBPPGEN_EXE_OBJS)
    IF EXIST "$(HBPPGEN_EXE)" $(DEL) "$(HBPPGEN_EXE)" > NUL
    $(CC) @&&!
$(CFLAGS)
-e$(HBPPGEN_EXE)
$(**: = ^
)
$(COMMON_LIB)
!
#**********************************************************
# HBPP build rule
#**********************************************************
$(HBPP_EXE) :: BasicLibs
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
$(STANDARD_STATIC_HBLIBS)
!
#**********************************************************
# HBDOT build rule
#**********************************************************
$(HBDOT_EXE)  :: BasicLibs BasicExes StdLibs
$(HBDOT_EXE)  :: $(HBDOT_EXE_OBJS)
    IF EXIST "$(HBDOT_EXE)" $(DEL) "$(HBDOT_EXE)" > NUL
    $(CC) @&&!
$(CFLAGS)
-e$(HBDOT_EXE)
$(**: = ^
)
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
$(STANDARD_STATIC_HBLIBS)
!
#**********************************************************
# HBPPTEST build rule
#**********************************************************
$(HBPPTEST_EXE) :: BasicLibs BasicExes StdLibs
$(HBPPTEST_EXE) :: $(HBPPTEST_EXE_OBJS)
    IF EXIST "$(HBPPTEST_EXE)" $(DEL) "$(HBPPTEST_EXE)" > NUL
    $(CC) @&&!
$(CFLAGS)
-e$(HBPPTEST_EXE)
$(**: = ^
)
$(STANDARD_STATIC_HBLIBS)
$(PP_LIB)
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
$(STANDARD_STATIC_HBLIBS)
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
$(STANDARD_STATIC_HBLIBS)
!
#**********************************************************
# HBVER build rule
#**********************************************************
$(HBVER_EXE)  :: BasicLibs BasicExes StdLibs
$(HBVER_EXE)  :: $(HBVER_EXE_OBJS)
    IF EXIST "$(HBVER_EXE)" $(DEL) "$(HBVER_EXE)" > NUL
    $(CC) @&&!
$(CFLAGS)
-e$(HBVER_EXE)
$(**: = ^
)
$(STANDARD_STATIC_HBLIBS)
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
$(HBTESTDLL_EXE) :: $(DLL_OBJ_DIR)\mainstd.obj $(HBTEST_EXE_OBJS:$(OBJ_DIR)=$(DLL_OBJ_DIR))
    $(LINKER) $(LDFLAGS) @&&!
c0x32.obj $**, $@,,$(HARBOUR_DLL:.dll=.lib) cw32mt$(RTLIBSUFFIX).lib import32.lib
!
#----------------------------------------------------------
#$(DLL_OBJ_DIR)\hbtest.obj : $(HBTEST_DIR)\hbtest.prg
#    $(HB) $(HARBOURFLAGS) -o$(DLL_OBJ_DIR)\  $**
#    $(CC) $(CLIBFLAGSDLL) -o$@ $(DLL_OBJ_DIR)\$&.c
#----------------------------------------------------------
$(DLL_OBJ_DIR)\mainstd.obj : $(VM_DIR)\mainstd.c
    $(CC) $(CEXEFLAGSDLL) -o$@ $**
#**********************************************************

#**********************************************************
# EXTRA Object's DEPENDENCIES
#**********************************************************

#**********************************************************

# Generated by an intermediate utility hbppgen.exe
# built at the initial phase of build process
$(OBJ_DIR)\pptable.obj     : $(OBJ_DIR)\pptable.c
$(DLL_OBJ_DIR)\pptable.obj : $(DLL_OBJ_DIR)\pptable.c

$(OBJ_DIR)\pptable.c     : include\hbstdgen.ch include\std.ch $(PP_DIR)\ppcore.c $(PP_DIR)\hbppgen.c
    IF EXIST "$(OBJ_DIR)\pptable.c" $(DEL) "$(OBJ_DIR)\pptable.c" > nul
    $(HBPPGEN) include/hbstdgen.ch -o$(OBJ_DIR)/pptable.c -q

$(DLL_OBJ_DIR)\pptable.c : include\hbstdgen.ch include\std.ch $(PP_DIR)\ppcore.c $(PP_DIR)\hbppgen.c
    IF EXIST "$(DLL_OBJ_DIR)\pptable.c" $(DEL) "$(DLL_OBJ_DIR)\pptable.c" > nul
    $(HBPPGEN) include/hbstdgen.ch -o$(DLL_OBJ_DIR)/pptable.c -q

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
    -if exist $(BIN_DIR)\*.exe $(DEL) $(BIN_DIR)\*.exe > nul
    -if exist $(BIN_DIR)\*.tds $(DEL) $(BIN_DIR)\*.tds > nul
    -if exist $(BIN_DIR)\*.tr? $(DEL) $(BIN_DIR)\*.tr? > nul
    -if exist $(BIN_DIR)\*.map $(DEL) $(BIN_DIR)\*.map > nul
    -if exist $(BIN_DIR)\*.dll $(DEL) $(BIN_DIR)\*.dll > nul
    -if exist $(BIN_DIR)\*.lib $(DEL) $(BIN_DIR)\*.lib > nul
    -if exist $(LIB_DIR)\*.lib $(DEL) $(LIB_DIR)\*.lib > nul
    -if exist $(LIB_DIR)\*.bak $(DEL) $(LIB_DIR)\*.bak > nul
    -if exist $(OBJ_DIR)\*.obj $(DEL) $(OBJ_DIR)\*.obj > nul
    -if exist $(OBJ_DIR)\*.c   $(DEL) $(OBJ_DIR)\*.c   > nul
    -if exist $(OBJ_DIR)\*.h   $(DEL) $(OBJ_DIR)\*.h   > nul
    -if exist $(DLL_OBJ_DIR)\*.obj   $(DEL) $(DLL_OBJ_DIR)\*.obj   > nul
    -if exist $(DLL_OBJ_DIR)\*.c     $(DEL) $(DLL_OBJ_DIR)\*.c     > nul
    -if exist $(DLL_OBJ_DIR)\*.h     $(DEL) $(DLL_OBJ_DIR)\*.h     > nul
    -if exist inst_$(HB_CC_NAME).log $(DEL) inst_$(HB_CC_NAME).log > nul
    -if exist gtlibs.mak       $(DEL) gtlibs.mak       > nul
    -if exist bin\*.exe        $(DEL) bin\*.exe        > nul
    -if exist lib\*.lib        $(DEL) lib\*.lib        > nul

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
    -if exist $(HB_INC_INSTALL)\nul   copy /A include\*.api $(HB_INC_INSTALL) >> inst_$(HB_CC_NAME).log
    -if exist $(HB_INC_INSTALL)\nul   copy /A include\*.ch  $(HB_INC_INSTALL) >> inst_$(HB_CC_NAME).log
    -if exist $(HB_INC_INSTALL)\nul   copy /A include\*.h   $(HB_INC_INSTALL) >> inst_$(HB_CC_NAME).log
!endif

#**********************************************************
