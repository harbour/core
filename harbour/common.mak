#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2007 Marek Paliwoda (mpaliwoda "at" interia "dot" pl)
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

#**********************************************************
# Common makefile.bc and makefile.vc definitions
#**********************************************************

#
# Macro to show/hide executed commands
#
!if "$(HB_BUILD_VERBOSE)" != "yes"
.SILENT:
!endif

#**********************************************************

# ---------------------------------------------------------------
# "echo." intentionally used instead of "echo", to avoid conflicts
# with external commands named echo.
# using macros for ECHO and DEL to allow overiding such as:
#
#    set ECHO=cmd /c echo
#    set DEL=cmd /c del
#
# The above might be needed on Windows 2000 and XP.
# The macros are referenced in makefile.bc
# ---------------------------------------------------------------

!ifndef ECHO
ECHO = echo.
!endif
!ifndef DEL
DEL = del
!endif

#**********************************************************

#
# binary file suffixes and prefixes
#
!ifndef OBJEXT
OBJEXT=.obj
!endif

!ifndef EXEEXT
EXEEXT=.exe
!endif

!ifndef DLLEXT
DLLEXT=.dll
!endif

!ifndef LIBEXT
LIBEXT=.lib
!endif

!ifndef LIBPREF
LIBPREF=
!endif

#**********************************************************
# Install directory defaults.
#**********************************************************

!ifndef HB_INSTALL_PREFIX
HB_INSTALL_PREFIX = .
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

!ifdef _HB_CC_NAME
HB_CC_NAME = $(_HB_CC_NAME)
!endif

BIN_DIR = bin\$(HB_CC_NAME)
OBJ_DIR = obj\$(HB_CC_NAME)
LIB_DIR = lib\$(HB_CC_NAME)

DLL_ROOTDIR = obj\dll
DLL_OBJ_DIR = $(DLL_ROOTDIR)\$(HB_CC_NAME)

# Targets Destination Directories
HB_DEST_DIRS = \
    $(BIN_DIR)      \
    $(OBJ_DIR)      \
    $(LIB_DIR)      \
    \
    $(DLL_ROOTDIR)  \
    $(DLL_OBJ_DIR)

MAIN_DIR     = source\main
COMMON_DIR   = source\common
COMPILER_DIR = source\compiler
PP_DIR       = source\pp
VM_DIR       = source\vm
RTL_DIR      = source\rtl
MACRO_DIR    = source\macro
DEBUG_DIR    = source\debug
LANG_DIR     = source\lang
CODEPAGE_DIR = source\codepage
PCRE_DIR     = source\hbpcre
RDD_DIR      = source\rdd
NULSYS_DIR   = source\rdd\nulsys
DBFNTX_DIR   = source\rdd\dbfntx
DBFCDX_DIR   = source\rdd\dbfcdx
DBFFPT_DIR   = source\rdd\dbffpt
HBSIX_DIR    = source\rdd\hbsix
HSX_DIR      = source\rdd\hsx
USRRDD_DIR   = source\rdd\usrrdd

GTCGI_DIR    = source\rtl\gtcgi
GTSTD_DIR    = source\rtl\gtstd
GTPCA_DIR    = source\rtl\gtpca
#---
GTDOS_DIR    = source\rtl\gtdos
#---
GTWIN_DIR    = source\rtl\gtwin
GTWVT_DIR    = source\rtl\gtwvt
GTGUI_DIR    = source\rtl\gtgui
#---
GTTRM_DIR    = source\rtl\gttrm
GTCRS_DIR    = source\rtl\gtcrs
GTSLN_DIR    = source\rtl\gtsln
GTXWC_DIR    = source\rtl\gtxwc

HBPP_DIR     = utils\hbpp
HBPPTEST_DIR = utils\hbpptest
HBRUN_DIR    = utils\hbrun
HBDOT_DIR    = utils\hbdot
HBTEST_DIR   = utils\hbtest
HBDOC_DIR    = utils\hbdoc
HBMAKE_DIR   = utils\hbmake
HBVER_DIR    = utils\hbver

!ifdef HB_DOC_PDF
HBPDF_DIR   = contrib\pdflib
!endif

#**********************************************************

# Where Bcc-Make should look for C and PRG sources
ALL_LIB_SRC_DIRS_TMP=\
$(OBJ_DIR);\
$(MAIN_DIR);\
$(COMMON_DIR);\
$(COMPILER_DIR);\
$(PP_DIR);\
$(VM_DIR);\
$(RTL_DIR);\
$(MACRO_DIR);\
$(DEBUG_DIR);\
$(LANG_DIR);\
$(CODEPAGE_DIR);\
$(PCRE_DIR);\
$(RDD_DIR);\
$(NULSYS_DIR);\
$(DBFNTX_DIR);\
$(DBFCDX_DIR);\
$(DBFFPT_DIR);\
$(HBSIX_DIR);\
$(HSX_DIR);\
$(USRRDD_DIR);\
$(GTCGI_DIR);\
$(GTSTD_DIR);\
$(GTPCA_DIR);\
$(GTWIN_DIR);\
$(GTWVT_DIR);\
$(GTGUI_DIR);\
$(GTTRM_DIR);\
$(GTCRS_DIR);\
$(GTSLN_DIR);\
$(GTXWC_DIR)\

ALL_EXE_SRC_DIRS_TMP=\
$(HBPPTEST_DIR);\
$(HBRUN_DIR);\
$(HBDOT_DIR);\
$(HBTEST_DIR);\
$(HBDOC_DIR);\
$(HBMAKE_DIR);\
$(HBVER_DIR)\

ALL_SRC_DIRS_TMP=\
$(ALL_LIB_SRC_DIRS_TMP);\
$(HBPP_DIR);\
$(ALL_EXE_SRC_DIRS_TMP)\

ALL_LIB_SRC_DIRS = $(ALL_LIB_SRC_DIRS_TMP: =)
ALL_EXE_SRC_DIRS = $(ALL_EXE_SRC_DIRS_TMP: =)
ALL_SRC_DIRS     = $(ALL_SRC_DIRS_TMP: =)

#**********************************************************
#**********************************************************
#**********************************************************

#
# Macros to define our library and executable names
#

COMMON_LIB   = $(LIB_DIR)\$(LIBPREF)common$(LIBEXT)
COMPILER_LIB = $(LIB_DIR)\$(LIBPREF)compiler$(LIBEXT)
PP_LIB       = $(LIB_DIR)\$(LIBPREF)pp$(LIBEXT)
VM_LIB       = $(LIB_DIR)\$(LIBPREF)vm$(LIBEXT)
RTL_LIB      = $(LIB_DIR)\$(LIBPREF)rtl$(LIBEXT)
MACRO_LIB    = $(LIB_DIR)\$(LIBPREF)macro$(LIBEXT)
DEBUG_LIB    = $(LIB_DIR)\$(LIBPREF)debug$(LIBEXT)
LANG_LIB     = $(LIB_DIR)\$(LIBPREF)lang$(LIBEXT)
CODEPAGE_LIB = $(LIB_DIR)\$(LIBPREF)codepage$(LIBEXT)
PCRE_LIB     = $(LIB_DIR)\$(LIBPREF)hbpcre$(LIBEXT)
RDD_LIB      = $(LIB_DIR)\$(LIBPREF)rdd$(LIBEXT)
NULSYS_LIB   = $(LIB_DIR)\$(LIBPREF)nulsys$(LIBEXT)
DBFNTX_LIB   = $(LIB_DIR)\$(LIBPREF)dbfntx$(LIBEXT)
DBFCDX_LIB   = $(LIB_DIR)\$(LIBPREF)dbfcdx$(LIBEXT)
DBFFPT_LIB   = $(LIB_DIR)\$(LIBPREF)dbffpt$(LIBEXT)
HBSIX_LIB    = $(LIB_DIR)\$(LIBPREF)hbsix$(LIBEXT)
HSX_LIB      = $(LIB_DIR)\$(LIBPREF)hsx$(LIBEXT)
USRRDD_LIB   = $(LIB_DIR)\$(LIBPREF)usrrdd$(LIBEXT)

GTCGI_LIB    = $(LIB_DIR)\$(LIBPREF)gtcgi$(LIBEXT)
GTSTD_LIB    = $(LIB_DIR)\$(LIBPREF)gtstd$(LIBEXT)
#---
GTDOS_LIB    = $(LIB_DIR)\$(LIBPREF)gtdos$(LIBEXT)
GTPCA_LIB    = $(LIB_DIR)\$(LIBPREF)gtpca$(LIBEXT)
GTWIN_LIB    = $(LIB_DIR)\$(LIBPREF)gtwin$(LIBEXT)
GTWVT_LIB    = $(LIB_DIR)\$(LIBPREF)gtwvt$(LIBEXT)
GTGUI_LIB    = $(LIB_DIR)\$(LIBPREF)gtgui$(LIBEXT)
#---
GTTRM_LIB    = $(LIB_DIR)\$(LIBPREF)gttrm$(LIBEXT)
GTCRS_LIB    = $(LIB_DIR)\$(LIBPREF)gtcrs$(LIBEXT)
GTSLN_LIB    = $(LIB_DIR)\$(LIBPREF)gtsln$(LIBEXT)
GTXWC_LIB    = $(LIB_DIR)\$(LIBPREF)gtxwc$(LIBEXT)

HARBOUR_EXE  = $(BIN_DIR)\harbour$(EXEEXT)
# required (intermediate) utility
#     to generate pptable.c
HBPPGEN_EXE  = $(BIN_DIR)\hbppgen$(EXEEXT)
HBPP_EXE     = $(BIN_DIR)\hbpp$(EXEEXT)
HBPPTEST_EXE = $(BIN_DIR)\hbpptest$(EXEEXT)
HBRUN_EXE    = $(BIN_DIR)\hbrun$(EXEEXT)
HBDOT_EXE    = $(BIN_DIR)\hbdot$(EXEEXT)
HBTEST_EXE   = $(BIN_DIR)\hbtest$(EXEEXT)
HBDOC_EXE    = $(BIN_DIR)\hbdoc$(EXEEXT)
HBMAKE_EXE   = $(BIN_DIR)\hbmake$(EXEEXT)
HBVER_EXE    = $(BIN_DIR)\hbverfix$(EXEEXT)

HARBOUR_DLL  = $(BIN_DIR)\$(LIBPREF)harbour-$(HB_CC_NAME)$(DLLEXT)
HBTESTDLL_EXE= $(BIN_DIR)\hbtest-dll$(EXEEXT)

#**********************************************************

!ifndef HB_GT_LIBS

#
# GT drivers supported by all platforms
#
HB_STD_GT = $(GTCGI_LIB) $(GTSTD_LIB) $(GTPCA_LIB)

#
# WinOS's GT driver list
#
HB_WINOS_GT = \
    $(GTWIN_LIB) \
    $(GTWVT_LIB) \
    $(GTGUI_LIB)

!ifdef HB_GT_LIST
# Hack - (HB_GT_LIST) is replaced by make_gcc.sh
# when it creates common.cf - a modified verion
# of common.mak
HB_GT_LIBS = $(HB_STD_GT) $(HB_GT_LIST)
!else
HB_GT_LIBS = $(HB_STD_GT) $(HB_WINOS_GT)
!endif

!endif

!if "$(HB_GT_LIB)" == ""
HB_GT_LIB = gtstd
!endif

#**********************************************************
#**********************************************************
#**********************************************************

# Standard Libs for HB-based executables
STANDARD_STATIC_HBLIBS = \
    $(COMMON_LIB)        \
    $(PP_LIB)            \
    $(COMPILER_LIB)      \
    $(VM_LIB)            \
    $(RTL_LIB)           \
    $(HB_GT_LIBS)        \
    $(LANG_LIB)          \
    $(CODEPAGE_LIB)      \
    $(PCRE_LIB)          \
    $(RDD_LIB)           \
    $(MACRO_LIB)         \
    $(DEBUG_LIB)         \
    $(DBFNTX_LIB)        \
    $(DBFCDX_LIB)        \
    $(DBFFPT_LIB)        \
    $(HBSIX_LIB)         \
    $(HSX_LIB)           \
    $(USRRDD_LIB)        \

#**********************************************************
#**********************************************************
#**********************************************************

# OBJECT LIST definitions

#**********************************************************

COMMON_LIB_OBJS = \
    $(OBJ_DIR)\expropt1$(OBJEXT) \
    $(OBJ_DIR)\expropt2$(OBJEXT) \
    $(OBJ_DIR)\hbarch$(OBJEXT)   \
    $(OBJ_DIR)\hbfhnd$(OBJEXT)   \
    $(OBJ_DIR)\hbfsapi$(OBJEXT)  \
    $(OBJ_DIR)\hbfopen$(OBJEXT)  \
    $(OBJ_DIR)\hbgete$(OBJEXT)   \
    $(OBJ_DIR)\hbwince$(OBJEXT)  \
    $(OBJ_DIR)\hbhash$(OBJEXT)   \
    $(OBJ_DIR)\hbdate$(OBJEXT)   \
    $(OBJ_DIR)\hbstr$(OBJEXT)    \
    $(OBJ_DIR)\hbtrace$(OBJEXT)  \
    $(OBJ_DIR)\hbver$(OBJEXT)    \
    $(OBJ_DIR)\hbverdsp$(OBJEXT) \
    $(OBJ_DIR)\reserved$(OBJEXT)

#**********************************************************

PP_LIB_OBJS = \
    $(OBJ_DIR)\pptable$(OBJEXT)  \
    $(OBJ_DIR)\ppcore$(OBJEXT)   \
    $(OBJ_DIR)\pplib$(OBJEXT)    \
    $(OBJ_DIR)\pplib2$(OBJEXT)   \
    $(OBJ_DIR)\pplib3$(OBJEXT)   \

#**********************************************************

COMPILER_LIB_OBJS = \
    $(OBJ_DIR)\hbmain$(OBJEXT)   \
    $(OBJ_DIR)\harboury$(OBJEXT) \
    $(OBJ_DIR)\complex$(OBJEXT)  \
    $(OBJ_DIR)\cmdcheck$(OBJEXT) \
    $(OBJ_DIR)\hbcomp$(OBJEXT)   \
    $(OBJ_DIR)\hbcmplib$(OBJEXT) \
    $(OBJ_DIR)\hbdbginf$(OBJEXT) \
    $(OBJ_DIR)\hbdead$(OBJEXT)   \
    $(OBJ_DIR)\hbstripl$(OBJEXT) \
    $(OBJ_DIR)\hbusage$(OBJEXT)  \
    $(OBJ_DIR)\hbident$(OBJEXT)  \
    $(OBJ_DIR)\hbgenerr$(OBJEXT) \
    $(OBJ_DIR)\hblbl$(OBJEXT)    \
    $(OBJ_DIR)\hbpcode$(OBJEXT)  \
    $(OBJ_DIR)\hbfunchk$(OBJEXT) \
    $(OBJ_DIR)\hbfix$(OBJEXT)    \
    $(OBJ_DIR)\hbopt$(OBJEXT)    \
    $(OBJ_DIR)\ppcomp$(OBJEXT)   \
    $(OBJ_DIR)\genc$(OBJEXT)     \
    $(OBJ_DIR)\gencc$(OBJEXT)    \
    $(OBJ_DIR)\gencobj$(OBJEXT)  \
    $(OBJ_DIR)\genobj32$(OBJEXT) \
    $(OBJ_DIR)\genhrb$(OBJEXT)   \
    $(OBJ_DIR)\expropta$(OBJEXT) \
    $(OBJ_DIR)\exproptb$(OBJEXT) \

#**********************************************************

# VM Objects common for STATIC and SHARED library
VM_COMMON_LIB_OBJS = \
    $(OBJ_DIR)\arrays$(OBJEXT)   \
    $(OBJ_DIR)\arrayshb$(OBJEXT) \
    $(OBJ_DIR)\asort$(OBJEXT)    \
    $(OBJ_DIR)\break$(OBJEXT)    \
    $(OBJ_DIR)\classes$(OBJEXT)  \
    $(OBJ_DIR)\cmdarg$(OBJEXT)   \
    $(OBJ_DIR)\codebloc$(OBJEXT) \
    $(OBJ_DIR)\debug$(OBJEXT)    \
    $(OBJ_DIR)\dynlibhb$(OBJEXT) \
    $(OBJ_DIR)\dynsym$(OBJEXT)   \
    $(OBJ_DIR)\estack$(OBJEXT)   \
    $(OBJ_DIR)\eval$(OBJEXT)     \
    $(OBJ_DIR)\evalhb$(OBJEXT)   \
    $(OBJ_DIR)\extend$(OBJEXT)   \
    $(OBJ_DIR)\fm$(OBJEXT)       \
    $(OBJ_DIR)\garbage$(OBJEXT)  \
    $(OBJ_DIR)\hashes$(OBJEXT)   \
    $(OBJ_DIR)\hashfunc$(OBJEXT) \
    $(OBJ_DIR)\hvm$(OBJEXT)      \
    $(OBJ_DIR)\initexit$(OBJEXT) \
    $(OBJ_DIR)\initsymb$(OBJEXT) \
    $(OBJ_DIR)\itemapi$(OBJEXT)  \
    $(OBJ_DIR)\macro$(OBJEXT)    \
    $(OBJ_DIR)\memvars$(OBJEXT)  \
    $(OBJ_DIR)\memvclip$(OBJEXT) \
    $(OBJ_DIR)\pcount$(OBJEXT)   \
    $(OBJ_DIR)\proc$(OBJEXT)     \
    $(OBJ_DIR)\pvalue$(OBJEXT)   \
    $(OBJ_DIR)\runner$(OBJEXT)   \
    $(OBJ_DIR)\vm$(OBJEXT)       \
    $(OBJ_DIR)\harbinit$(OBJEXT) \

# Specific VM Objects for building STATIC library
VM_STATIC_LIB_OBJS = \
    $(OBJ_DIR)\mainstd$(OBJEXT)  \
    $(OBJ_DIR)\mainwin$(OBJEXT)

# Specific VM Objects for building SHARED (DLL) library
VM_SHARED_LIB_OBJS = \
    $(OBJ_DIR)\maindllh$(OBJEXT) \

# All VM Objects for building STATIC library
VM_LIB_OBJS = $(VM_COMMON_LIB_OBJS) $(VM_STATIC_LIB_OBJS)

# All VM Objects for building SHARED (DLL) library
VM_DLL_OBJS = $(VM_COMMON_LIB_OBJS) $(VM_SHARED_LIB_OBJS)

DISABLED_VM_OBJS = \
    $(OBJ_DIR)\maindllp$(OBJEXT) \
    $(OBJ_DIR)\maindll$(OBJEXT)  \

#**********************************************************

RTL_LIB_OBJS = \
    $(OBJ_DIR)\abs$(OBJEXT)      \
    $(OBJ_DIR)\accept$(OBJEXT)   \
    $(OBJ_DIR)\ampm$(OBJEXT)     \
    $(OBJ_DIR)\at$(OBJEXT)       \
    $(OBJ_DIR)\binnum$(OBJEXT)   \
    $(OBJ_DIR)\binnumx$(OBJEXT)  \
    $(OBJ_DIR)\box$(OBJEXT)      \
    $(OBJ_DIR)\cdpapi$(OBJEXT)   \
    $(OBJ_DIR)\chrasc$(OBJEXT)   \
    $(OBJ_DIR)\colorind$(OBJEXT) \
    $(OBJ_DIR)\console$(OBJEXT)  \
    $(OBJ_DIR)\copyfile$(OBJEXT) \
    $(OBJ_DIR)\datec$(OBJEXT)    \
    $(OBJ_DIR)\dates$(OBJEXT)    \
    $(OBJ_DIR)\dateshb$(OBJEXT)  \
    $(OBJ_DIR)\datesx$(OBJEXT)   \
    $(OBJ_DIR)\defpath$(OBJEXT)  \
    $(OBJ_DIR)\descend$(OBJEXT)  \
    $(OBJ_DIR)\dirdrive$(OBJEXT) \
    $(OBJ_DIR)\direct$(OBJEXT)   \
    $(OBJ_DIR)\diskspac$(OBJEXT) \
    $(OBJ_DIR)\disksphb$(OBJEXT) \
    $(OBJ_DIR)\do$(OBJEXT)       \
    $(OBJ_DIR)\empty$(OBJEXT)    \
    $(OBJ_DIR)\errorapi$(OBJEXT) \
    $(OBJ_DIR)\errorint$(OBJEXT) \
    $(OBJ_DIR)\file$(OBJEXT)     \
    $(OBJ_DIR)\filehb$(OBJEXT)   \
    $(OBJ_DIR)\filesys$(OBJEXT)  \
    $(OBJ_DIR)\fkmax$(OBJEXT)    \
    $(OBJ_DIR)\fnsplit$(OBJEXT)  \
    $(OBJ_DIR)\fserror$(OBJEXT)  \
    $(OBJ_DIR)\fssize$(OBJEXT)   \
    $(OBJ_DIR)\fstemp$(OBJEXT)   \
    $(OBJ_DIR)\gete$(OBJEXT)     \
    $(OBJ_DIR)\gt$(OBJEXT)       \
    $(OBJ_DIR)\gtapi$(OBJEXT)    \
    $(OBJ_DIR)\gtchrmap$(OBJEXT) \
    $(OBJ_DIR)\gtapiu$(OBJEXT)   \
    $(OBJ_DIR)\gtclip$(OBJEXT)   \
    $(OBJ_DIR)\gtfunc$(OBJEXT)   \
    $(OBJ_DIR)\gtkbstat$(OBJEXT) \
    $(OBJ_DIR)\gtsys$(OBJEXT)    \
    $(OBJ_DIR)\gttone$(OBJEXT)   \
    $(OBJ_DIR)\gx$(OBJEXT)       \
    $(OBJ_DIR)\hardcr$(OBJEXT)   \
    $(OBJ_DIR)\hbadler$(OBJEXT)  \
    $(OBJ_DIR)\hbbit$(OBJEXT)    \
    $(OBJ_DIR)\hbcrc$(OBJEXT)    \
    $(OBJ_DIR)\hbhex$(OBJEXT)    \
    $(OBJ_DIR)\hbmd5$(OBJEXT)    \
    $(OBJ_DIR)\hbffind$(OBJEXT)  \
    $(OBJ_DIR)\hbfile$(OBJEXT)   \
    $(OBJ_DIR)\hbgtcore$(OBJEXT) \
    $(OBJ_DIR)\hbinet$(OBJEXT)   \
    $(OBJ_DIR)\hbstrsh$(OBJEXT)  \
    $(OBJ_DIR)\hbrandom$(OBJEXT) \
    $(OBJ_DIR)\hbregex$(OBJEXT)  \
    $(OBJ_DIR)\hbregexc$(OBJEXT) \
    $(OBJ_DIR)\hbtoken$(OBJEXT)  \
    $(OBJ_DIR)\idle$(OBJEXT)     \
    $(OBJ_DIR)\inkey$(OBJEXT)    \
    $(OBJ_DIR)\is$(OBJEXT)       \
    $(OBJ_DIR)\isprint$(OBJEXT)  \
    $(OBJ_DIR)\itemseri$(OBJEXT) \
    $(OBJ_DIR)\langapi$(OBJEXT)  \
    $(OBJ_DIR)\left$(OBJEXT)     \
    $(OBJ_DIR)\len$(OBJEXT)      \
    $(OBJ_DIR)\lennum$(OBJEXT)   \
    $(OBJ_DIR)\math$(OBJEXT)     \
    $(OBJ_DIR)\maxrow$(OBJEXT)   \
    $(OBJ_DIR)\memofile$(OBJEXT) \
    $(OBJ_DIR)\minmax$(OBJEXT)   \
    $(OBJ_DIR)\mlcfunc$(OBJEXT)  \
    $(OBJ_DIR)\mod$(OBJEXT)      \
    $(OBJ_DIR)\mouse53$(OBJEXT)  \
    $(OBJ_DIR)\mouseapi$(OBJEXT) \
    $(OBJ_DIR)\mousex$(OBJEXT)   \
    $(OBJ_DIR)\mtran$(OBJEXT)    \
    $(OBJ_DIR)\natmsg$(OBJEXT)   \
    $(OBJ_DIR)\natmsgu$(OBJEXT)  \
    $(OBJ_DIR)\net$(OBJEXT)      \
    $(OBJ_DIR)\oemansi$(OBJEXT)  \
    $(OBJ_DIR)\oemansix$(OBJEXT) \
    $(OBJ_DIR)\oldbox$(OBJEXT)   \
    $(OBJ_DIR)\oldclear$(OBJEXT) \
    $(OBJ_DIR)\pad$(OBJEXT)      \
    $(OBJ_DIR)\padc$(OBJEXT)     \
    $(OBJ_DIR)\padl$(OBJEXT)     \
    $(OBJ_DIR)\padr$(OBJEXT)     \
    $(OBJ_DIR)\philes$(OBJEXT)   \
    $(OBJ_DIR)\philes53$(OBJEXT) \
    $(OBJ_DIR)\philesx$(OBJEXT)  \
    $(OBJ_DIR)\rat$(OBJEXT)      \
    $(OBJ_DIR)\replic$(OBJEXT)   \
    $(OBJ_DIR)\right$(OBJEXT)    \
    $(OBJ_DIR)\round$(OBJEXT)    \
    $(OBJ_DIR)\run$(OBJEXT)      \
    $(OBJ_DIR)\samples$(OBJEXT)  \
    $(OBJ_DIR)\saverest$(OBJEXT) \
    $(OBJ_DIR)\scroll$(OBJEXT)   \
    $(OBJ_DIR)\secondfs$(OBJEXT) \
    $(OBJ_DIR)\seconds$(OBJEXT)  \
    $(OBJ_DIR)\set$(OBJEXT)      \
    $(OBJ_DIR)\setcolor$(OBJEXT) \
    $(OBJ_DIR)\setcurs$(OBJEXT)  \
    $(OBJ_DIR)\setkey$(OBJEXT)   \
    $(OBJ_DIR)\setpos$(OBJEXT)   \
    $(OBJ_DIR)\setposbs$(OBJEXT) \
    $(OBJ_DIR)\shadow$(OBJEXT)   \
    $(OBJ_DIR)\soundex$(OBJEXT)  \
    $(OBJ_DIR)\space$(OBJEXT)    \
    $(OBJ_DIR)\spfiles$(OBJEXT)  \
    $(OBJ_DIR)\str$(OBJEXT)      \
    $(OBJ_DIR)\strpeek$(OBJEXT)  \
    $(OBJ_DIR)\strcase$(OBJEXT)  \
    $(OBJ_DIR)\strings$(OBJEXT)  \
    $(OBJ_DIR)\strmatch$(OBJEXT) \
    $(OBJ_DIR)\strtran$(OBJEXT)  \
    $(OBJ_DIR)\strzero$(OBJEXT)  \
    $(OBJ_DIR)\stuff$(OBJEXT)    \
    $(OBJ_DIR)\substr$(OBJEXT)   \
    $(OBJ_DIR)\tone$(OBJEXT)     \
    $(OBJ_DIR)\trace$(OBJEXT)    \
    $(OBJ_DIR)\transfrm$(OBJEXT) \
    $(OBJ_DIR)\trim$(OBJEXT)     \
    $(OBJ_DIR)\tscalara$(OBJEXT) \
    $(OBJ_DIR)\tscalarb$(OBJEXT) \
    $(OBJ_DIR)\tscalarc$(OBJEXT) \
    $(OBJ_DIR)\tscalard$(OBJEXT) \
    $(OBJ_DIR)\tscalarh$(OBJEXT) \
    $(OBJ_DIR)\tscalarl$(OBJEXT) \
    $(OBJ_DIR)\tscalarn$(OBJEXT) \
    $(OBJ_DIR)\tscalarp$(OBJEXT) \
    $(OBJ_DIR)\tscalars$(OBJEXT) \
    $(OBJ_DIR)\tscalaru$(OBJEXT) \
    $(OBJ_DIR)\type$(OBJEXT)     \
    $(OBJ_DIR)\val$(OBJEXT)      \
    $(OBJ_DIR)\valtostr$(OBJEXT) \
    $(OBJ_DIR)\valtype$(OBJEXT)  \
    $(OBJ_DIR)\version$(OBJEXT)  \
    $(OBJ_DIR)\word$(OBJEXT)     \
    $(OBJ_DIR)\xhelp$(OBJEXT)    \
    $(OBJ_DIR)\xsavescr$(OBJEXT) \
    \
    $(OBJ_DIR)\achoice$(OBJEXT)  \
    $(OBJ_DIR)\adir$(OBJEXT)     \
    $(OBJ_DIR)\alert$(OBJEXT)    \
    $(OBJ_DIR)\altd$(OBJEXT)     \
    $(OBJ_DIR)\browdb$(OBJEXT)   \
    $(OBJ_DIR)\browdbx$(OBJEXT)  \
    $(OBJ_DIR)\browse$(OBJEXT)   \
    $(OBJ_DIR)\checkbox$(OBJEXT) \
    $(OBJ_DIR)\color53$(OBJEXT)  \
    $(OBJ_DIR)\dbedit$(OBJEXT)   \
    $(OBJ_DIR)\devoutp$(OBJEXT)  \
    $(OBJ_DIR)\dircmd$(OBJEXT)   \
    $(OBJ_DIR)\einstvar$(OBJEXT) \
    $(OBJ_DIR)\errorsys$(OBJEXT) \
    $(OBJ_DIR)\fieldbl$(OBJEXT)  \
    $(OBJ_DIR)\getlist$(OBJEXT)  \
    $(OBJ_DIR)\getsys$(OBJEXT)   \
    $(OBJ_DIR)\getsys53$(OBJEXT) \
    $(OBJ_DIR)\gui$(OBJEXT)      \
    $(OBJ_DIR)\hbini$(OBJEXT)    \
    $(OBJ_DIR)\input$(OBJEXT)    \
    $(OBJ_DIR)\listbox$(OBJEXT)  \
    $(OBJ_DIR)\memoedit$(OBJEXT) \
    $(OBJ_DIR)\memvarbl$(OBJEXT) \
    $(OBJ_DIR)\menuto$(OBJEXT)   \
    $(OBJ_DIR)\menusys$(OBJEXT)  \
    $(OBJ_DIR)\objfunc$(OBJEXT)  \
    $(OBJ_DIR)\perfuncs$(OBJEXT) \
    $(OBJ_DIR)\persist$(OBJEXT)  \
    $(OBJ_DIR)\profiler$(OBJEXT) \
    $(OBJ_DIR)\pushbtn$(OBJEXT)  \
    $(OBJ_DIR)\radiobtn$(OBJEXT) \
    $(OBJ_DIR)\radiogrp$(OBJEXT) \
    $(OBJ_DIR)\readkey$(OBJEXT)  \
    $(OBJ_DIR)\readvar$(OBJEXT)  \
    $(OBJ_DIR)\scrollbr$(OBJEXT) \
    $(OBJ_DIR)\setfunc$(OBJEXT)  \
    $(OBJ_DIR)\setta$(OBJEXT)    \
    $(OBJ_DIR)\symbol$(OBJEXT)   \
    $(OBJ_DIR)\tbcolumn$(OBJEXT) \
    $(OBJ_DIR)\tbrowse$(OBJEXT)  \
    $(OBJ_DIR)\tbrowsys$(OBJEXT) \
    $(OBJ_DIR)\tclass$(OBJEXT)   \
    $(OBJ_DIR)\teditor$(OBJEXT)  \
    $(OBJ_DIR)\text$(OBJEXT)     \
    $(OBJ_DIR)\tget$(OBJEXT)     \
    $(OBJ_DIR)\tgetint$(OBJEXT)  \
    $(OBJ_DIR)\tgetlist$(OBJEXT) \
    $(OBJ_DIR)\tlabel$(OBJEXT)   \
    $(OBJ_DIR)\tmenuitm$(OBJEXT) \
    $(OBJ_DIR)\tmenusys$(OBJEXT) \
    $(OBJ_DIR)\tobject$(OBJEXT)  \
    $(OBJ_DIR)\tpopup$(OBJEXT)   \
    $(OBJ_DIR)\treport$(OBJEXT)  \
    $(OBJ_DIR)\tscalar$(OBJEXT)  \
    $(OBJ_DIR)\ttextlin$(OBJEXT) \
    $(OBJ_DIR)\ttopbar$(OBJEXT)  \
    $(OBJ_DIR)\typefile$(OBJEXT) \
    $(OBJ_DIR)\typefilx$(OBJEXT) \
    $(OBJ_DIR)\valtoexp$(OBJEXT) \
    $(OBJ_DIR)\wait$(OBJEXT)     \

#**********************************************************

MACRO_LIB_OBJS = \
    $(OBJ_DIR)\macroy$(OBJEXT)   \
    $(OBJ_DIR)\macroa$(OBJEXT)   \
    $(OBJ_DIR)\macrob$(OBJEXT)   \
    $(OBJ_DIR)\macrolex$(OBJEXT) \

#**********************************************************

DEBUG_LIB_OBJS = \
    $(OBJ_DIR)\dbgentry$(OBJEXT) \
    $(OBJ_DIR)\dbgbrwsr$(OBJEXT) \
    $(OBJ_DIR)\dbghelp$(OBJEXT)  \
    $(OBJ_DIR)\dbgmenu$(OBJEXT)  \
    $(OBJ_DIR)\dbgtmenu$(OBJEXT) \
    $(OBJ_DIR)\dbgtmitm$(OBJEXT) \
    $(OBJ_DIR)\dbgtwin$(OBJEXT)  \
    $(OBJ_DIR)\debugger$(OBJEXT) \
    $(OBJ_DIR)\dbgtarr$(OBJEXT)  \
    $(OBJ_DIR)\dbgtobj$(OBJEXT)  \
    $(OBJ_DIR)\dbgthsh$(OBJEXT)  \
    $(OBJ_DIR)\tbrwtext$(OBJEXT) \
    $(OBJ_DIR)\dbgwa$(OBJEXT)    \

#**********************************************************

LANG_LIB_OBJS = \
    $(OBJ_DIR)\msgbg866$(OBJEXT) \
    $(OBJ_DIR)\msgbgiso$(OBJEXT) \
    $(OBJ_DIR)\msgbgwin$(OBJEXT) \
    $(OBJ_DIR)\msgca$(OBJEXT)    \
    $(OBJ_DIR)\msgcs852$(OBJEXT) \
    $(OBJ_DIR)\msgcsiso$(OBJEXT) \
    $(OBJ_DIR)\msgcskam$(OBJEXT) \
    $(OBJ_DIR)\msgcswin$(OBJEXT) \
    $(OBJ_DIR)\msgde$(OBJEXT)    \
    $(OBJ_DIR)\msgdewin$(OBJEXT) \
    $(OBJ_DIR)\msgel$(OBJEXT)    \
    $(OBJ_DIR)\msgelwin$(OBJEXT) \
    $(OBJ_DIR)\msgeo$(OBJEXT)    \
    $(OBJ_DIR)\msges$(OBJEXT)    \
    $(OBJ_DIR)\msgeswin$(OBJEXT) \
    $(OBJ_DIR)\msgeu$(OBJEXT)    \
    $(OBJ_DIR)\msgfr$(OBJEXT)    \
    $(OBJ_DIR)\msggl$(OBJEXT)    \
    $(OBJ_DIR)\msghe862$(OBJEXT) \
    $(OBJ_DIR)\msghewin$(OBJEXT) \
    $(OBJ_DIR)\msghr852$(OBJEXT) \
    $(OBJ_DIR)\msghriso$(OBJEXT) \
    $(OBJ_DIR)\msghu852$(OBJEXT) \
    $(OBJ_DIR)\msghucwi$(OBJEXT) \
    $(OBJ_DIR)\msghuiso$(OBJEXT) \
    $(OBJ_DIR)\msghuwin$(OBJEXT) \
    $(OBJ_DIR)\msgid$(OBJEXT)    \
    $(OBJ_DIR)\msgis850$(OBJEXT) \
    $(OBJ_DIR)\msgit$(OBJEXT)    \
    $(OBJ_DIR)\msgko$(OBJEXT)    \
    $(OBJ_DIR)\msgnl$(OBJEXT)    \
    $(OBJ_DIR)\msgpl852$(OBJEXT) \
    $(OBJ_DIR)\msgpliso$(OBJEXT) \
    $(OBJ_DIR)\msgplmaz$(OBJEXT) \
    $(OBJ_DIR)\msgplwin$(OBJEXT) \
    $(OBJ_DIR)\msgpt$(OBJEXT)    \
    $(OBJ_DIR)\msgro$(OBJEXT)    \
    $(OBJ_DIR)\msgru866$(OBJEXT) \
    $(OBJ_DIR)\msgrukoi$(OBJEXT) \
    $(OBJ_DIR)\msgruwin$(OBJEXT) \
    $(OBJ_DIR)\msgsl437$(OBJEXT) \
    $(OBJ_DIR)\msgsl852$(OBJEXT) \
    $(OBJ_DIR)\msgsliso$(OBJEXT) \
    $(OBJ_DIR)\msgslwin$(OBJEXT) \
    $(OBJ_DIR)\msgsr852$(OBJEXT) \
    $(OBJ_DIR)\msgsriso$(OBJEXT) \
    $(OBJ_DIR)\msgsrwin$(OBJEXT) \
    $(OBJ_DIR)\msgtrdos$(OBJEXT) \
    $(OBJ_DIR)\msgtrwin$(OBJEXT) \
    $(OBJ_DIR)\msgzhb5$(OBJEXT)  \
    $(OBJ_DIR)\msgzhgb$(OBJEXT)  \

#**********************************************************
PCRE_LIB_OBJS = \
    $(OBJ_DIR)\chartabs$(OBJEXT) \
    $(OBJ_DIR)\pcrecomp$(OBJEXT) \
    $(OBJ_DIR)\pcreconf$(OBJEXT) \
    $(OBJ_DIR)\pcredfa$(OBJEXT)  \
    $(OBJ_DIR)\pcreexec$(OBJEXT) \
    $(OBJ_DIR)\pcrefinf$(OBJEXT) \
    $(OBJ_DIR)\pcreget$(OBJEXT)  \
    $(OBJ_DIR)\pcreglob$(OBJEXT) \
    $(OBJ_DIR)\pcreinfo$(OBJEXT) \
    $(OBJ_DIR)\pcremktb$(OBJEXT) \
    $(OBJ_DIR)\pcreoutf$(OBJEXT) \
    $(OBJ_DIR)\pcreprni$(OBJEXT) \
    $(OBJ_DIR)\pcrerefc$(OBJEXT) \
    $(OBJ_DIR)\pcrestud$(OBJEXT) \
    $(OBJ_DIR)\pcretabs$(OBJEXT) \
    $(OBJ_DIR)\pcretryf$(OBJEXT) \
    $(OBJ_DIR)\pcrefind$(OBJEXT) \
    $(OBJ_DIR)\pcrevutf$(OBJEXT) \
    $(OBJ_DIR)\pcrever$(OBJEXT)  \
    $(OBJ_DIR)\pcrexcls$(OBJEXT) \

#**********************************************************

CODEPAGE_LIB_OBJS = \
    $(OBJ_DIR)\cpbg866$(OBJEXT)  \
    $(OBJ_DIR)\cpbgiso$(OBJEXT)  \
    $(OBJ_DIR)\cpbgmik$(OBJEXT)  \
    $(OBJ_DIR)\cpbgwin$(OBJEXT)  \
    $(OBJ_DIR)\cpcs852$(OBJEXT)  \
    $(OBJ_DIR)\cpcsiso$(OBJEXT)  \
    $(OBJ_DIR)\cpcskam$(OBJEXT)  \
    $(OBJ_DIR)\cpcswin$(OBJEXT)  \
    $(OBJ_DIR)\cpeldos$(OBJEXT)  \
    $(OBJ_DIR)\cpelwin$(OBJEXT)  \
    $(OBJ_DIR)\cpesdos$(OBJEXT)  \
    $(OBJ_DIR)\cpesmwin$(OBJEXT) \
    $(OBJ_DIR)\cpeswin$(OBJEXT)  \
    $(OBJ_DIR)\cpfrdos$(OBJEXT)  \
    $(OBJ_DIR)\cpgedos$(OBJEXT)  \
    $(OBJ_DIR)\cpgewin$(OBJEXT)  \
    $(OBJ_DIR)\cphr1250$(OBJEXT) \
    $(OBJ_DIR)\cphr437$(OBJEXT)  \
    $(OBJ_DIR)\cphr852$(OBJEXT)  \
    $(OBJ_DIR)\cphu852$(OBJEXT)  \
    $(OBJ_DIR)\cphu852s$(OBJEXT) \
    $(OBJ_DIR)\cphuiso$(OBJEXT)  \
    $(OBJ_DIR)\cphuisos$(OBJEXT) \
    $(OBJ_DIR)\cphuwin$(OBJEXT)  \
    $(OBJ_DIR)\cphuwins$(OBJEXT) \
    $(OBJ_DIR)\cpit437$(OBJEXT)  \
    $(OBJ_DIR)\cpit850$(OBJEXT)  \
    $(OBJ_DIR)\cpitisb$(OBJEXT)  \
    $(OBJ_DIR)\cpitiso$(OBJEXT)  \
    $(OBJ_DIR)\cpltwin$(OBJEXT)  \
    $(OBJ_DIR)\cppl852$(OBJEXT)  \
    $(OBJ_DIR)\cppliso$(OBJEXT)  \
    $(OBJ_DIR)\cpplmaz$(OBJEXT)  \
    $(OBJ_DIR)\cpplwin$(OBJEXT)  \
    $(OBJ_DIR)\cppt850$(OBJEXT)  \
    $(OBJ_DIR)\cpptiso$(OBJEXT)  \
    $(OBJ_DIR)\cpru866$(OBJEXT)  \
    $(OBJ_DIR)\cprukoi$(OBJEXT)  \
    $(OBJ_DIR)\cpruwin$(OBJEXT)  \
    $(OBJ_DIR)\cpsk852$(OBJEXT)  \
    $(OBJ_DIR)\cpskiso$(OBJEXT)  \
    $(OBJ_DIR)\cpskkam$(OBJEXT)  \
    $(OBJ_DIR)\cpskwin$(OBJEXT)  \
    $(OBJ_DIR)\cpsl437$(OBJEXT)  \
    $(OBJ_DIR)\cpsl852$(OBJEXT)  \
    $(OBJ_DIR)\cpsliso$(OBJEXT)  \
    $(OBJ_DIR)\cpslwin$(OBJEXT)  \
    $(OBJ_DIR)\cpsrwin$(OBJEXT)  \
    $(OBJ_DIR)\cpsv850$(OBJEXT)  \
    $(OBJ_DIR)\cpsvclip$(OBJEXT) \
    $(OBJ_DIR)\cpsvwin$(OBJEXT)  \
    $(OBJ_DIR)\cptrdos$(OBJEXT)  \
    $(OBJ_DIR)\cptrwin$(OBJEXT)  \
    $(OBJ_DIR)\cpua866$(OBJEXT)  \
    $(OBJ_DIR)\cpuakoi$(OBJEXT)  \
    $(OBJ_DIR)\cpuawin$(OBJEXT)  \
    $(OBJ_DIR)\uc1250$(OBJEXT)   \
    $(OBJ_DIR)\uc1251$(OBJEXT)   \
    $(OBJ_DIR)\uc1253$(OBJEXT)   \
    $(OBJ_DIR)\uc1254$(OBJEXT)   \
    $(OBJ_DIR)\uc1257$(OBJEXT)   \
    $(OBJ_DIR)\uc737$(OBJEXT)    \
    $(OBJ_DIR)\uc850$(OBJEXT)    \
    $(OBJ_DIR)\uc852$(OBJEXT)    \
    $(OBJ_DIR)\uc857$(OBJEXT)    \
    $(OBJ_DIR)\uc866$(OBJEXT)    \
    $(OBJ_DIR)\uc8859_1$(OBJEXT) \
    $(OBJ_DIR)\uc8859_2$(OBJEXT) \
    $(OBJ_DIR)\uc8859_5$(OBJEXT) \
    $(OBJ_DIR)\uc88591b$(OBJEXT) \
    $(OBJ_DIR)\uckoi8$(OBJEXT)   \
    $(OBJ_DIR)\uckoi8u$(OBJEXT)  \
    $(OBJ_DIR)\ucmaz$(OBJEXT)    \
    $(OBJ_DIR)\uckam$(OBJEXT)    \
    $(OBJ_DIR)\ucmik$(OBJEXT)    \

#**********************************************************

RDD_LIB_OBJS = \
    $(OBJ_DIR)\dbcmd$(OBJEXT)    \
    $(OBJ_DIR)\dbcmd53$(OBJEXT)  \
    $(OBJ_DIR)\dbcmdx$(OBJEXT)   \
    $(OBJ_DIR)\dbdrop$(OBJEXT)   \
    $(OBJ_DIR)\dbexists$(OBJEXT) \
    $(OBJ_DIR)\fieldhb$(OBJEXT)  \
    $(OBJ_DIR)\hbdbsort$(OBJEXT) \
    $(OBJ_DIR)\workarea$(OBJEXT) \
    $(OBJ_DIR)\wacore$(OBJEXT)   \
    $(OBJ_DIR)\wafunc$(OBJEXT)   \
    $(OBJ_DIR)\dbf1$(OBJEXT)     \
    $(OBJ_DIR)\dbnubs$(OBJEXT)   \
    $(OBJ_DIR)\delim1$(OBJEXT)   \
    $(OBJ_DIR)\dbsql$(OBJEXT)    \
    $(OBJ_DIR)\sdf1$(OBJEXT)     \
    $(OBJ_DIR)\rddinfo$(OBJEXT)  \
    \
    $(OBJ_DIR)\dbdelim$(OBJEXT)  \
    $(OBJ_DIR)\dbsdf$(OBJEXT)    \
    $(OBJ_DIR)\dbjoin$(OBJEXT)   \
    $(OBJ_DIR)\dbjoinx$(OBJEXT)  \
    $(OBJ_DIR)\dbtotal$(OBJEXT)  \
    $(OBJ_DIR)\dbtotalx$(OBJEXT) \
    $(OBJ_DIR)\dbfuncs$(OBJEXT)  \
    $(OBJ_DIR)\dbfuncsx$(OBJEXT) \
    $(OBJ_DIR)\dblist$(OBJEXT)   \
    $(OBJ_DIR)\dblistx$(OBJEXT)  \
    $(OBJ_DIR)\dbsort$(OBJEXT)   \
    $(OBJ_DIR)\dbsortx$(OBJEXT)  \
    $(OBJ_DIR)\dbstrux$(OBJEXT)  \
    $(OBJ_DIR)\dbstruxx$(OBJEXT) \
    $(OBJ_DIR)\dbupdat$(OBJEXT)  \
    $(OBJ_DIR)\dbupdatx$(OBJEXT) \
    $(OBJ_DIR)\rddord$(OBJEXT)   \
    $(OBJ_DIR)\rddsys$(OBJEXT)   \

#**********************************************************

NULSYS_LIB_OBJS = \
    $(OBJ_DIR)\nulsys$(OBJEXT)

#**********************************************************

DBFNTX_LIB_OBJS = \
    $(OBJ_DIR)\dbfntx1$(OBJEXT)  \
    $(OBJ_DIR)\dbfntx0$(OBJEXT)  \

#**********************************************************

DBFCDX_LIB_OBJS = \
    $(OBJ_DIR)\dbfcdx1$(OBJEXT)  \
    $(OBJ_DIR)\sixcdx1$(OBJEXT)  \

#**********************************************************

DBFFPT_LIB_OBJS = \
    $(OBJ_DIR)\dbffpt1$(OBJEXT)  \

#**********************************************************

HBSIX_LIB_OBJS = \
    $(OBJ_DIR)\sxcompr$(OBJEXT)  \
    $(OBJ_DIR)\sxcrypt$(OBJEXT)  \
    $(OBJ_DIR)\sxdate$(OBJEXT)   \

#**********************************************************

HSX_LIB_OBJS = \
    $(OBJ_DIR)\hsx$(OBJEXT)      \
    $(OBJ_DIR)\cftsfunc$(OBJEXT) \

#**********************************************************

USRRDD_LIB_OBJS = \
    $(OBJ_DIR)\usrrdd$(OBJEXT)   \

#**********************************************************

GTCGI_LIB_OBJS = \
    $(OBJ_DIR)\gtcgi$(OBJEXT)    \

#**********************************************************

GTSTD_LIB_OBJS = \
    $(OBJ_DIR)\gtstd$(OBJEXT)    \

#**********************************************************

GTDOS_LIB_OBJS = \
    $(OBJ_DIR)\gtdos$(OBJEXT)    \

#**********************************************************

GTPCA_LIB_OBJS = \
    $(OBJ_DIR)\gtpca$(OBJEXT)    \

#**********************************************************

GTWIN_LIB_OBJS = \
    $(OBJ_DIR)\gtwin$(OBJEXT)    \

#**********************************************************

GTWVT_LIB_OBJS = \
    $(OBJ_DIR)\gtwvt$(OBJEXT)    \

#**********************************************************

GTGUI_LIB_COMMON_OBJS = \
    $(OBJ_DIR)\gtgui$(OBJEXT)    \

GTGUI_LIB_STATIC_OBJS = \
    $(OBJ_DIR)\gtdef$(OBJEXT)    \

GTGUI_LIB_SHARED_OBJS = \

GTGUI_LIB_OBJS = $(GTGUI_LIB_COMMON_OBJS) $(GTGUI_LIB_STATIC_OBJS)
GTGUI_DLL_OBJS = $(GTGUI_LIB_COMMON_OBJS) $(GTGUI_LIB_SHARED_OBJS)

#**********************************************************

GTTRM_LIB_OBJS = \
    $(OBJ_DIR)\gttrm$(OBJEXT)    \

#**********************************************************

GTCRS_LIB_OBJS = \
    $(OBJ_DIR)\gtcrs$(OBJEXT)    \

#**********************************************************

GTSLN_LIB_OBJS = \
    $(OBJ_DIR)\gtsln$(OBJEXT)    \
    $(OBJ_DIR)\kbsln$(OBJEXT)    \
    $(OBJ_DIR)\mousesln$(OBJEXT) \

#**********************************************************

GTXWC_LIB_OBJS = \
    $(OBJ_DIR)\gtxwc$(OBJEXT)    \

#**********************************************************
#**********************************************************
#**********************************************************

HARBOUR_EXE_OBJS = \
    $(OBJ_DIR)\harbour$(OBJEXT)  \

#**********************************************************

HBPP_EXE_OBJS = \
    $(OBJ_DIR)\hbpp$(OBJEXT)     \
    $(OBJ_DIR)\hbpptbl$(OBJEXT)  \
    $(OBJ_DIR)\hbppcomp$(OBJEXT) \
    $(OBJ_DIR)\hbppcore$(OBJEXT) \
    $(OBJ_DIR)\pragma$(OBJEXT)   \

#    $(OBJ_DIR)\hbpplib$(OBJEXT)  \

#**********************************************************

HBPPGEN_EXE_OBJS = \
    $(OBJ_DIR)\hbppgen$(OBJEXT)  \

#**********************************************************

HBPPTEST_EXE_OBJS = \
    $(OBJ_DIR)\pretest$(OBJEXT)  \

#**********************************************************

HBRUN_EXE_OBJS = \
    $(OBJ_DIR)\hbrun$(OBJEXT)    \
    $(OBJ_DIR)\external$(OBJEXT) \

#**********************************************************

HBDOT_EXE_OBJS = \
    $(OBJ_DIR)\hbdot$(OBJEXT)    \

#**********************************************************

HBTEST_EXE_OBJS = \
    $(OBJ_DIR)\hbtest$(OBJEXT)   \
    $(OBJ_DIR)\rt_hvm$(OBJEXT)   \
    $(OBJ_DIR)\rt_hvma$(OBJEXT)  \
    $(OBJ_DIR)\rt_math$(OBJEXT)  \
    $(OBJ_DIR)\rt_date$(OBJEXT)  \
    $(OBJ_DIR)\rt_str$(OBJEXT)   \
    $(OBJ_DIR)\rt_stra$(OBJEXT)  \
    $(OBJ_DIR)\rt_trans$(OBJEXT) \
    $(OBJ_DIR)\rt_array$(OBJEXT) \
    $(OBJ_DIR)\rt_file$(OBJEXT)  \
    $(OBJ_DIR)\rt_misc$(OBJEXT)  \
    $(OBJ_DIR)\rt_class$(OBJEXT) \

#**********************************************************

HBDOC_EXE_OBJS = \
    $(OBJ_DIR)\hbdoc$(OBJEXT)    \
    $(OBJ_DIR)\genasc$(OBJEXT)   \
    $(OBJ_DIR)\genhpc$(OBJEXT)   \
    $(OBJ_DIR)\genhtm$(OBJEXT)   \
    $(OBJ_DIR)\genchm$(OBJEXT)   \
    $(OBJ_DIR)\genng$(OBJEXT)    \
    $(OBJ_DIR)\genos2$(OBJEXT)   \
    $(OBJ_DIR)\genrtf$(OBJEXT)   \
    $(OBJ_DIR)\gentrf$(OBJEXT)   \
    $(OBJ_DIR)\teeasc$(OBJEXT)   \
    $(OBJ_DIR)\html$(OBJEXT)     \
    $(OBJ_DIR)\ng$(OBJEXT)       \
    $(OBJ_DIR)\os2$(OBJEXT)      \
    $(OBJ_DIR)\rtf$(OBJEXT)      \
    $(OBJ_DIR)\troff$(OBJEXT)    \
    $(OBJ_DIR)\fclass1$(OBJEXT)  \
    $(OBJ_DIR)\ffile1$(OBJEXT)   \
    $(OBJ_DIR)\ft_funcs$(OBJEXT) \

!ifdef HB_DOC_PDF

# PDF support for HBDOC
HBDOC_EXE_OBJS = \
    $(HBDOC_EXE_OBJS)       \
    $(OBJ_DIR)\pdfhbdoc$(OBJEXT) \
    $(OBJ_DIR)\genpdf1$(OBJEXT)  \

!endif

#**********************************************************

HBMAKE_EXE_OBJS = \
    $(OBJ_DIR)\hbmake$(OBJEXT)   \
    $(OBJ_DIR)\hbmutils$(OBJEXT) \
    $(OBJ_DIR)\checks$(OBJEXT)   \
    $(OBJ_DIR)\pickarry$(OBJEXT) \
    $(OBJ_DIR)\pickfile$(OBJEXT) \
    $(OBJ_DIR)\prb_stak$(OBJEXT) \
    $(OBJ_DIR)\radios$(OBJEXT)   \
    $(OBJ_DIR)\fclass1$(OBJEXT)  \
    $(OBJ_DIR)\ffile1$(OBJEXT)   \
    $(OBJ_DIR)\ft_funcs$(OBJEXT) \
    $(OBJ_DIR)\hbmlang$(OBJEXT)  \
    $(OBJ_DIR)\readline$(OBJEXT) \
    $(OBJ_DIR)\tmake$(OBJEXT)    \

#**********************************************************

HBVER_EXE_OBJS = \
    $(OBJ_DIR)\hbverfix$(OBJEXT) \

#**********************************************************
#**********************************************************
#**********************************************************

#
# HARBOUR_DLL objects
#

# Here we create a temporary DLL obj variable
# with all objects required for building DLL.
# They have wrong OBJ directory. We fix it
# in each respective makefile.

#-------------------------

!ifdef HB_GT_LIST
# Hack - (HB_GT_OBJS) is replaced by make_gcc.sh
# when it creates common.cf - a modified verion
# of common.mak
DLL_GT_OBJS = $(HB_GT_OBJS)
!else
DLL_GT_OBJS = \
    $(GTWIN_LIB_OBJS)       \
    $(GTWVT_LIB_OBJS)       \
    $(GTGUI_DLL_OBJS)
!endif

#-------------------------

TMP_DLL_OBJS = \
    $(COMMON_LIB_OBJS)      \
    $(PP_LIB_OBJS)          \
    $(VM_DLL_OBJS)          \
    $(RTL_LIB_OBJS)         \
    $(MACRO_LIB_OBJS)       \
    $(DEBUG_LIB_OBJS)       \
    $(LANG_LIB_OBJS)        \
    $(CODEPAGE_LIB_OBJS)    \
    $(PCRE_LIB_OBJS)        \
    $(RDD_LIB_OBJS)         \
    $(DBFNTX_LIB_OBJS)      \
    $(DBFCDX_LIB_OBJS)      \
    $(DBFFPT_LIB_OBJS)      \
    $(HBSIX_LIB_OBJS)       \
    $(HSX_LIB_OBJS)         \
    $(USRRDD_LIB_OBJS)      \
    $(GTCGI_LIB_OBJS)       \
    $(GTPCA_LIB_OBJS)       \
    $(GTSTD_LIB_OBJS)       \
    $(DLL_GT_OBJS)          \

#-------------------------

DISABLED_SHARED_MODULES=    \
    $(NULSYS_LIB_OBJS)      \
    $(GTDOS_LIB_OBJS)       \

#**********************************************************
#**********************************************************
#**********************************************************

#
# Our default Targets
#

HB_BUILD_TARGETS = \
    $(COMMON_LIB)           \
    $(HBPPGEN_EXE)          \
    $(PP_LIB)               \
    \
    $(COMPILER_LIB)         \
    $(HARBOUR_EXE)          \
    $(HBPP_EXE)             \
    \
    $(VM_LIB)               \
    $(RTL_LIB)              \
    $(MACRO_LIB)            \
    $(DEBUG_LIB)            \
    $(LANG_LIB)             \
    $(CODEPAGE_LIB)         \
    $(PCRE_LIB)             \
    $(RDD_LIB)              \
    $(NULSYS_LIB)           \
    $(DBFNTX_LIB)           \
    $(DBFCDX_LIB)           \
    $(DBFFPT_LIB)           \
    $(HBSIX_LIB)            \
    $(HSX_LIB)              \
    $(USRRDD_LIB)           \
    $(HB_GT_LIBS)           \
    \
    $(HBRUN_EXE)            \
    $(HBDOT_EXE)            \
    $(HBTEST_EXE)           \
    $(HBPPTEST_EXE)         \
    $(HBDOC_EXE)            \
    $(HBMAKE_EXE)           \
    $(HBVER_EXE)            \

# DLL Target is disabled by default
# It can be enabled by setting env
# variable HB_BUILD_DLL to yes

!if "$(HB_BUILD_DLL)" == "yes"
HB_BUILD_TARGETS = $(HB_BUILD_TARGETS) $(HARBOUR_DLL) $(HBTESTDLL_EXE)
!endif

#**********************************************************
# Allows to do cross-compiling if neccesary.
#**********************************************************

!ifndef HB
HB = $(HARBOUR_EXE)
!endif

# allows to do cross-compiling if neccesary.
!ifndef HBPPGEN
HBPPGEN = $(HBPPGEN_EXE)
!endif

#**********************************************************
