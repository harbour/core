#
# $Id$
#

#**********************************************************
#
# Common makefile.bc and makefile.vc definitions
#
#**********************************************************

#
# Macro to show/hide executed commands
#
!if "$(HB_BUILD_VERBOSE)" != "yes"
.SILENT:
!endif

#**********************************************************

#
# binary file suffixes and prefixes
#
!ifndef HB_OBJ_EXT
HB_OBJ_EXT=.obj
!endif

!ifndef HB_EXE_EXT
HB_EXE_EXT=.exe
!endif

!ifndef HB_DLL_EXT
HB_DLL_EXT=.dll
!endif

!ifndef HB_LIB_EXT
HB_LIB_EXT=.lib
!endif

!ifndef HB_LIB_PREFIX
HB_LIB_PREFIX=
!endif

#**********************************************************

#
# Directory macros. These should never have to change.
#

BIN_DIR = bin\$(CC_DIRNAME)
OBJ_DIR = obj\$(CC_DIRNAME)
LIB_DIR = lib\$(CC_DIRNAME)

DLL_ROOTDIR = obj\dll
DLL_OBJ_DIR = $(DLL_ROOTDIR)\$(CC_DIRNAME)

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
GTDOS_DIR    = source\rtl\gtdos
GTPCA_DIR    = source\rtl\gtpca
GTSTD_DIR    = source\rtl\gtstd
GTWIN_DIR    = source\rtl\gtwin
GTWVT_DIR    = source\rtl\gtwvt
GTGUI_DIR    = source\rtl\gtgui

HARBOUR_DIR  = source\compiler
HBPP_DIR     = utils\hbpp
HBPPTEST_DIR = utils\hbpptest
HBRUN_DIR    = utils\hbrun
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
$(GTPCA_DIR);\
$(GTSTD_DIR);\
$(GTWIN_DIR);\
$(GTWVT_DIR);\
$(GTGUI_DIR)\

ALL_EXE_SRC_DIRS_TMP=\
$(HBPPTEST_DIR);\
$(HBRUN_DIR);\
$(HBTEST_DIR);\
$(HBDOC_DIR);\
$(HBMAKE_DIR);\
$(HBVER_DIR)\

ALL_SRC_DIRS_TMP=\
$(ALL_LIB_SRC_DIRS_TMP);\
$(HARBOUR_DIR);\
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

COMMON_LIB   = $(LIB_DIR)\$(HB_LIB_PREFIX)common$(HB_LIB_EXT)
COMPILER_LIB = $(LIB_DIR)\$(HB_LIB_PREFIX)compiler$(HB_LIB_EXT)
PP_LIB       = $(LIB_DIR)\$(HB_LIB_PREFIX)pp$(HB_LIB_EXT)
VM_LIB       = $(LIB_DIR)\$(HB_LIB_PREFIX)vm$(HB_LIB_EXT)
RTL_LIB      = $(LIB_DIR)\$(HB_LIB_PREFIX)rtl$(HB_LIB_EXT)
MACRO_LIB    = $(LIB_DIR)\$(HB_LIB_PREFIX)macro$(HB_LIB_EXT)
DEBUG_LIB    = $(LIB_DIR)\$(HB_LIB_PREFIX)debug$(HB_LIB_EXT)
LANG_LIB     = $(LIB_DIR)\$(HB_LIB_PREFIX)lang$(HB_LIB_EXT)
CODEPAGE_LIB = $(LIB_DIR)\$(HB_LIB_PREFIX)codepage$(HB_LIB_EXT)
PCRE_LIB     = $(LIB_DIR)\$(HB_LIB_PREFIX)hbpcre$(HB_LIB_EXT)
RDD_LIB      = $(LIB_DIR)\$(HB_LIB_PREFIX)rdd$(HB_LIB_EXT)
NULSYS_LIB   = $(LIB_DIR)\$(HB_LIB_PREFIX)nulsys$(HB_LIB_EXT)
DBFNTX_LIB   = $(LIB_DIR)\$(HB_LIB_PREFIX)dbfntx$(HB_LIB_EXT)
DBFCDX_LIB   = $(LIB_DIR)\$(HB_LIB_PREFIX)dbfcdx$(HB_LIB_EXT)
DBFFPT_LIB   = $(LIB_DIR)\$(HB_LIB_PREFIX)dbffpt$(HB_LIB_EXT)
HBSIX_LIB    = $(LIB_DIR)\$(HB_LIB_PREFIX)hbsix$(HB_LIB_EXT)
HSX_LIB      = $(LIB_DIR)\$(HB_LIB_PREFIX)hsx$(HB_LIB_EXT)
USRRDD_LIB   = $(LIB_DIR)\$(HB_LIB_PREFIX)usrrdd$(HB_LIB_EXT)

GTCGI_LIB    = $(LIB_DIR)\$(HB_LIB_PREFIX)gtcgi$(HB_LIB_EXT)
GTDOS_LIB    = $(LIB_DIR)\$(HB_LIB_PREFIX)gtdos$(HB_LIB_EXT)
GTPCA_LIB    = $(LIB_DIR)\$(HB_LIB_PREFIX)gtpca$(HB_LIB_EXT)
GTSTD_LIB    = $(LIB_DIR)\$(HB_LIB_PREFIX)gtstd$(HB_LIB_EXT)
GTWIN_LIB    = $(LIB_DIR)\$(HB_LIB_PREFIX)gtwin$(HB_LIB_EXT)
GTWVT_LIB    = $(LIB_DIR)\$(HB_LIB_PREFIX)gtwvt$(HB_LIB_EXT)
GTGUI_LIB    = $(LIB_DIR)\$(HB_LIB_PREFIX)gtgui$(HB_LIB_EXT)

HARBOUR_EXE  = $(BIN_DIR)\harbour$(HB_EXE_EXT)
# required (intermediate) utility
#     to generate pptable.c
HBPPGEN_EXE  = $(BIN_DIR)\ppgen$(HB_EXE_EXT)
HBPP_EXE     = $(BIN_DIR)\hbpp$(HB_EXE_EXT)
HBPPTEST_EXE = $(BIN_DIR)\hbpptest$(HB_EXE_EXT)
HBRUN_EXE    = $(BIN_DIR)\hbrun$(HB_EXE_EXT)
HBTEST_EXE   = $(BIN_DIR)\hbtest$(HB_EXE_EXT)
HBDOC_EXE    = $(BIN_DIR)\hbdoc$(HB_EXE_EXT)
HBMAKE_EXE   = $(BIN_DIR)\hbmake$(HB_EXE_EXT)
HBVER_EXE    = $(BIN_DIR)\hbverfix$(HB_EXE_EXT)

HARBOUR_DLL  = $(BIN_DIR)\harbour-$(CC_DIRNAME)$(HB_DLL_EXT)
HBTESTDLL_EXE= $(BIN_DIR)\hbtest-dll$(HB_EXE_EXT)

#**********************************************************

#
# WinOS's GT driver list
#

HB_GT_LIBS = \
    $(GTCGI_LIB) \
    $(GTPCA_LIB) \
    $(GTSTD_LIB) \
    $(GTWIN_LIB) \
    $(GTWVT_LIB) \
    $(GTGUI_LIB)

!ifndef HB_GT_LIB
HB_GT_LIB = $(GTWIN_LIB)
!else
HB_GT_LIB = $(LIB_DIR)\$(HB_LIB_PREFIX)$(HB_GT_LIB)$(HB_LIB_EXT)
!endif

#**********************************************************
#**********************************************************
#**********************************************************

# Standard Libs for HB-based executables
STANDARD_STATIC_HBLIBS = \
    $(COMMON_LIB)     \
    $(COMPILER_LIB)   \
    $(PP_LIB)         \
    $(VM_LIB)         \
    $(RTL_LIB)        \
    $(HB_GT_LIB)      \
    $(LANG_LIB)       \
    $(CODEPAGE_LIB)   \
    $(PCRE_LIB)       \
    $(RDD_LIB)        \
    $(MACRO_LIB)      \
    $(DEBUG_LIB)      \
    $(DBFNTX_LIB)     \
    $(DBFCDX_LIB)     \
    $(DBFFPT_LIB)     \
    $(HBSIX_LIB)      \
    $(HSX_LIB)        \
    $(USRRDD_LIB)     \

#**********************************************************
#**********************************************************
#**********************************************************

# OBJECT LIST definitions

#**********************************************************

COMMON_LIB_OBJS = \
    $(OBJ_DIR)\expropt1$(HB_OBJ_EXT) \
    $(OBJ_DIR)\expropt2$(HB_OBJ_EXT) \
    $(OBJ_DIR)\hbarch$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\hbfhnd$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\hbfsapi$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\hbgete$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\hbhash$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\hbdate$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\hbstr$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\hbtrace$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\hbver$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\hbverdsp$(HB_OBJ_EXT) \
    $(OBJ_DIR)\reserved$(HB_OBJ_EXT)

#**********************************************************

PP_LIB_OBJS = \
    $(OBJ_DIR)\pptable$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\ppcore$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\pplib$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\pplib2$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\pplib3$(HB_OBJ_EXT)   \

#**********************************************************

COMPILER_LIB_OBJS = \
    $(OBJ_DIR)\hbmain$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\harboury$(HB_OBJ_EXT) \
    $(OBJ_DIR)\complex$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cmdcheck$(HB_OBJ_EXT) \
    $(OBJ_DIR)\hbcomp$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\hbcmplib$(HB_OBJ_EXT) \
    $(OBJ_DIR)\hbdbginf$(HB_OBJ_EXT) \
    $(OBJ_DIR)\hbdead$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\hbstripl$(HB_OBJ_EXT) \
    $(OBJ_DIR)\hbusage$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\hbident$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\hbgenerr$(HB_OBJ_EXT) \
    $(OBJ_DIR)\hblbl$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\hbpcode$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\hbfunchk$(HB_OBJ_EXT) \
    $(OBJ_DIR)\hbfix$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\hbopt$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\ppcomp$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\genc$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\gencc$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\gencli$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\gencobj$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\genobj32$(HB_OBJ_EXT) \
    $(OBJ_DIR)\genjava$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\genhrb$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\expropta$(HB_OBJ_EXT) \
    $(OBJ_DIR)\exproptb$(HB_OBJ_EXT) \

#**********************************************************

# VM Objects common for STATIC and SHARED library
VM_COMMON_LIB_OBJS = \
    $(OBJ_DIR)\arrays$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\arrayshb$(HB_OBJ_EXT) \
    $(OBJ_DIR)\asort$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\break$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\classes$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cmdarg$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\codebloc$(HB_OBJ_EXT) \
    $(OBJ_DIR)\debug$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\dynlibhb$(HB_OBJ_EXT) \
    $(OBJ_DIR)\dynsym$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\estack$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\eval$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\evalhb$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\extend$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\fm$(HB_OBJ_EXT)       \
    $(OBJ_DIR)\garbage$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\hashes$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\hashfunc$(HB_OBJ_EXT) \
    $(OBJ_DIR)\hvm$(HB_OBJ_EXT)      \
    $(OBJ_DIR)\initexit$(HB_OBJ_EXT) \
    $(OBJ_DIR)\initsymb$(HB_OBJ_EXT) \
    $(OBJ_DIR)\itemapi$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\macro$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\memvars$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\memvclip$(HB_OBJ_EXT) \
    $(OBJ_DIR)\pcount$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\proc$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\pvalue$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\runner$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\harbinit$(HB_OBJ_EXT) \

# Specific VM Objects for building STATIC library
VM_STATIC_LIB_OBJS = \
    $(OBJ_DIR)\mainstd$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\mainwin$(HB_OBJ_EXT)

# Specific VM Objects for building SHARED (DLL) library
VM_SHARED_LIB_OBJS = \
    $(OBJ_DIR)\maindllh$(HB_OBJ_EXT) \

# All VM Objects for building STATIC library
VM_LIB_OBJS = $(VM_COMMON_LIB_OBJS) $(VM_STATIC_LIB_OBJS)

# All VM Objects for building SHARED (DLL) library
VM_DLL_OBJS = $(VM_COMMON_LIB_OBJS) $(VM_SHARED_LIB_OBJS)

DISABLED_VM_OBJS = \
    $(OBJ_DIR)\maindllp$(HB_OBJ_EXT) \
    $(OBJ_DIR)\maindll$(HB_OBJ_EXT)  \

#**********************************************************

RTL_LIB_OBJS = \
    $(OBJ_DIR)\abs$(HB_OBJ_EXT)      \
    $(OBJ_DIR)\accept$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\ampm$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\at$(HB_OBJ_EXT)       \
    $(OBJ_DIR)\binnum$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\binnumx$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\box$(HB_OBJ_EXT)      \
    $(OBJ_DIR)\cdpapi$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\chrasc$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\colorind$(HB_OBJ_EXT) \
    $(OBJ_DIR)\console$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\copyfile$(HB_OBJ_EXT) \
    $(OBJ_DIR)\datec$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\dates$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\dateshb$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\datesx$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\defpath$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\descend$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\dirdrive$(HB_OBJ_EXT) \
    $(OBJ_DIR)\direct$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\diskspac$(HB_OBJ_EXT) \
    $(OBJ_DIR)\disksphb$(HB_OBJ_EXT) \
    $(OBJ_DIR)\do$(HB_OBJ_EXT)       \
    $(OBJ_DIR)\empty$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\errorapi$(HB_OBJ_EXT) \
    $(OBJ_DIR)\errorint$(HB_OBJ_EXT) \
    $(OBJ_DIR)\file$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\filehb$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\filesys$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\fkmax$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\fnsplit$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\fserror$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\fssize$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\fstemp$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\gete$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\gt$(HB_OBJ_EXT)       \
    $(OBJ_DIR)\gtapi$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\gtapiu$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\gtclip$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\gtfunc$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\gtsys$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\gttone$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\gx$(HB_OBJ_EXT)       \
    $(OBJ_DIR)\hardcr$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\hbadler$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\hbbit$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\hbcrc$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\hbhex$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\hbmd5$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\hbffind$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\hbgtcore$(HB_OBJ_EXT) \
    $(OBJ_DIR)\hbinet$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\hbrandom$(HB_OBJ_EXT) \
    $(OBJ_DIR)\hbregex$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\hbregexc$(HB_OBJ_EXT) \
    $(OBJ_DIR)\hbtoken$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\idle$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\inkey$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\is$(HB_OBJ_EXT)       \
    $(OBJ_DIR)\isprint$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\itemseri$(HB_OBJ_EXT) \
    $(OBJ_DIR)\langapi$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\left$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\len$(HB_OBJ_EXT)      \
    $(OBJ_DIR)\lennum$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\math$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\maxrow$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\memofile$(HB_OBJ_EXT) \
    $(OBJ_DIR)\minmax$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\mlcfunc$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\mod$(HB_OBJ_EXT)      \
    $(OBJ_DIR)\mouseapi$(HB_OBJ_EXT) \
    $(OBJ_DIR)\mousex$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\mtran$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\natmsg$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\net$(HB_OBJ_EXT)      \
    $(OBJ_DIR)\oemansi$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\oldbox$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\oldclear$(HB_OBJ_EXT) \
    $(OBJ_DIR)\pad$(HB_OBJ_EXT)      \
    $(OBJ_DIR)\padc$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\padl$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\padr$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\philes$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\philes53$(HB_OBJ_EXT) \
    $(OBJ_DIR)\philesx$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\rat$(HB_OBJ_EXT)      \
    $(OBJ_DIR)\replic$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\right$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\round$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\run$(HB_OBJ_EXT)      \
    $(OBJ_DIR)\samples$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\saverest$(HB_OBJ_EXT) \
    $(OBJ_DIR)\scroll$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\seconds$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\set$(HB_OBJ_EXT)      \
    $(OBJ_DIR)\setcolor$(HB_OBJ_EXT) \
    $(OBJ_DIR)\setcurs$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\setkey$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\setpos$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\setposbs$(HB_OBJ_EXT) \
    $(OBJ_DIR)\shadow$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\soundex$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\space$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\spfiles$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\str$(HB_OBJ_EXT)      \
    $(OBJ_DIR)\strpeek$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\strcase$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\strings$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\strmatch$(HB_OBJ_EXT) \
    $(OBJ_DIR)\strtran$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\strzero$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\stuff$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\substr$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\tone$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\trace$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\transfrm$(HB_OBJ_EXT) \
    $(OBJ_DIR)\trim$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\type$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\val$(HB_OBJ_EXT)      \
    $(OBJ_DIR)\valtostr$(HB_OBJ_EXT) \
    $(OBJ_DIR)\valtype$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\version$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\word$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\xhelp$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\xsavescr$(HB_OBJ_EXT) \
    \
    $(OBJ_DIR)\achoice$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\adir$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\alert$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\altd$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\array$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\block$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\browdb$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\browdbx$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\browse$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\characte$(HB_OBJ_EXT) \
    $(OBJ_DIR)\checkbox$(HB_OBJ_EXT) \
    $(OBJ_DIR)\color53$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\date$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\dbedit$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\devoutp$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\dircmd$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\errorsys$(HB_OBJ_EXT) \
    $(OBJ_DIR)\fieldbl$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\getlist$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\getsys$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\input$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\listbox$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\logical$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\memoedit$(HB_OBJ_EXT) \
    $(OBJ_DIR)\memvarbl$(HB_OBJ_EXT) \
    $(OBJ_DIR)\menuto$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\mssgline$(HB_OBJ_EXT) \
    $(OBJ_DIR)\nil$(HB_OBJ_EXT)      \
    $(OBJ_DIR)\numeric$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\objfunc$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\perfuncs$(HB_OBJ_EXT) \
    $(OBJ_DIR)\persist$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\profiler$(HB_OBJ_EXT) \
    $(OBJ_DIR)\pushbtn$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\radiobtn$(HB_OBJ_EXT) \
    $(OBJ_DIR)\radiogrp$(HB_OBJ_EXT) \
    $(OBJ_DIR)\readkey$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\readvar$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\scalar$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\scrollbr$(HB_OBJ_EXT) \
    $(OBJ_DIR)\setfunc$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\setta$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\symbol$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\tbcolumn$(HB_OBJ_EXT) \
    $(OBJ_DIR)\tbrowse$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\tclass$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\teditor$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\terror$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\text$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\tget$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\tgetint$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\tgetlist$(HB_OBJ_EXT) \
    $(OBJ_DIR)\tlabel$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\tmenuitm$(HB_OBJ_EXT) \
    $(OBJ_DIR)\tobject$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\tpopup$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\treport$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\ttextlin$(HB_OBJ_EXT) \
    $(OBJ_DIR)\ttopbar$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\typefile$(HB_OBJ_EXT) \
    $(OBJ_DIR)\valtoexp$(HB_OBJ_EXT) \
    $(OBJ_DIR)\wait$(HB_OBJ_EXT)     \

#**********************************************************

MACRO_LIB_OBJS = \
    $(OBJ_DIR)\macroy$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\macroa$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\macrob$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\macrolex$(HB_OBJ_EXT) \

#**********************************************************

DEBUG_LIB_OBJS = \
    $(OBJ_DIR)\dbgentry$(HB_OBJ_EXT) \
    $(OBJ_DIR)\dbgbrwsr$(HB_OBJ_EXT) \
    $(OBJ_DIR)\dbghelp$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\dbgmenu$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\dbgtmenu$(HB_OBJ_EXT) \
    $(OBJ_DIR)\dbgtmitm$(HB_OBJ_EXT) \
    $(OBJ_DIR)\dbgtwin$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\debugger$(HB_OBJ_EXT) \
    $(OBJ_DIR)\dbgtarr$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\dbgtobj$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\dbgthsh$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\tbrwtext$(HB_OBJ_EXT) \
    $(OBJ_DIR)\dbgwa$(HB_OBJ_EXT)    \

#**********************************************************

LANG_LIB_OBJS = \
    $(OBJ_DIR)\msgbg866$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgbgiso$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgbgwin$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgca$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\msgcs852$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgcsiso$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgcskam$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgcswin$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgde$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\msgdewin$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgel$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\msgelwin$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgeo$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\msges$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\msgeswin$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgeu$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\msgfr$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\msggl$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\msghe862$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msghewin$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msghr852$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msghriso$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msghu852$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msghucwi$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msghuiso$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msghuwin$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgid$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\msgis850$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgit$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\msgko$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\msgnl$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\msgpl852$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgpliso$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgplmaz$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgplwin$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgpt$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\msgro$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\msgru866$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgrukoi$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgruwin$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgsl437$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgsl852$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgsliso$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgslwin$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgsr852$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgsriso$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgsrwin$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgtrdos$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgtrwin$(HB_OBJ_EXT) \
    $(OBJ_DIR)\msgzhb5$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\msgzhgb$(HB_OBJ_EXT)  \

#**********************************************************
PCRE_LIB_OBJS = \
    $(OBJ_DIR)\chartabs$(HB_OBJ_EXT) \
    $(OBJ_DIR)\pcrecomp$(HB_OBJ_EXT) \
    $(OBJ_DIR)\pcreconf$(HB_OBJ_EXT) \
    $(OBJ_DIR)\pcredfa$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\pcreexec$(HB_OBJ_EXT) \
    $(OBJ_DIR)\pcrefinf$(HB_OBJ_EXT) \
    $(OBJ_DIR)\pcreget$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\pcreglob$(HB_OBJ_EXT) \
    $(OBJ_DIR)\pcreinfo$(HB_OBJ_EXT) \
    $(OBJ_DIR)\pcremktb$(HB_OBJ_EXT) \
    $(OBJ_DIR)\pcreoutf$(HB_OBJ_EXT) \
    $(OBJ_DIR)\pcreprni$(HB_OBJ_EXT) \
    $(OBJ_DIR)\pcrerefc$(HB_OBJ_EXT) \
    $(OBJ_DIR)\pcrestud$(HB_OBJ_EXT) \
    $(OBJ_DIR)\pcretabs$(HB_OBJ_EXT) \
    $(OBJ_DIR)\pcretryf$(HB_OBJ_EXT) \
    $(OBJ_DIR)\pcrefind$(HB_OBJ_EXT) \
    $(OBJ_DIR)\pcrevutf$(HB_OBJ_EXT) \
    $(OBJ_DIR)\pcrever$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\pcrexcls$(HB_OBJ_EXT) \

#**********************************************************

CODEPAGE_LIB_OBJS = \
    $(OBJ_DIR)\cpbg866$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpbgiso$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpbgwin$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpcs852$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpcsiso$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpcskam$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpcswin$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpeldos$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpelwin$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpesdos$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpesmwi$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpeswin$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpfrdos$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpgedos$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpgewin$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cphr1250$(HB_OBJ_EXT) \
    $(OBJ_DIR)\cphr437$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cphr852$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cphu852$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cphu852s$(HB_OBJ_EXT) \
    $(OBJ_DIR)\cphuiso$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cphuisos$(HB_OBJ_EXT) \
    $(OBJ_DIR)\cphuwin$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cphuwins$(HB_OBJ_EXT) \
    $(OBJ_DIR)\cpit437$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpit850$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpitisb$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpitiso$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpltwin$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cppl852$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cppliso$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpplmaz$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpplwin$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cppt850$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpptiso$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpru866$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cprukoi$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpruwin$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpsl437$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpsl852$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpsliso$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpslwin$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpsrwin$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cptrdos$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cptrwin$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpua866$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpuakoi$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\cpuawin$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\uc1250$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\uc1251$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\uc1253$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\uc1254$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\uc1257$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\uc737$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\uc850$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\uc852$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\uc857$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\uc866$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\uc8859_1$(HB_OBJ_EXT) \
    $(OBJ_DIR)\uc8859_2$(HB_OBJ_EXT) \
    $(OBJ_DIR)\uc8859_5$(HB_OBJ_EXT) \
    $(OBJ_DIR)\uc88591b$(HB_OBJ_EXT) \
    $(OBJ_DIR)\uckoi8$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\uckoi8u$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\ucmaz$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\uckam$(HB_OBJ_EXT)    \

#**********************************************************

RDD_LIB_OBJS = \
    $(OBJ_DIR)\dbcmd$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\workarea$(HB_OBJ_EXT) \
    $(OBJ_DIR)\wacore$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\wafunc$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\dbf1$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\dbnubs$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\dbsql$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\delim1$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\sdf1$(HB_OBJ_EXT)     \
    \
    $(OBJ_DIR)\dbdelim$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\dbsdf$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\dbjoin$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\dbtotal$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\dbfuncs$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\dblist$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\dbsort$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\hbdbsort$(HB_OBJ_EXT) \
    $(OBJ_DIR)\dbstrux$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\dbupdat$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\rddord$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\rddsys$(HB_OBJ_EXT)   \

#**********************************************************

NULSYS_LIB_OBJS = \
    $(OBJ_DIR)\nulsys$(HB_OBJ_EXT)

#**********************************************************

DBFNTX_LIB_OBJS = \
    $(OBJ_DIR)\dbfntx1$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\dbfntx0$(HB_OBJ_EXT)  \

#**********************************************************

DBFCDX_LIB_OBJS = \
    $(OBJ_DIR)\dbfcdx1$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\sixcdx1$(HB_OBJ_EXT)  \

#**********************************************************

DBFFPT_LIB_OBJS = \
    $(OBJ_DIR)\dbffpt1$(HB_OBJ_EXT)  \

#**********************************************************

HBSIX_LIB_OBJS = \
    $(OBJ_DIR)\sxcompr$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\sxcrypt$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\sxdate$(HB_OBJ_EXT)   \

#**********************************************************

HSX_LIB_OBJS = \
    $(OBJ_DIR)\hsx$(HB_OBJ_EXT)      \
    $(OBJ_DIR)\cftsfunc$(HB_OBJ_EXT) \

#**********************************************************

USRRDD_LIB_OBJS = \
    $(OBJ_DIR)\usrrdd$(HB_OBJ_EXT)   \

#**********************************************************

GTCGI_LIB_OBJS = \
    $(OBJ_DIR)\gtcgi$(HB_OBJ_EXT)    \

#**********************************************************

GTDOS_LIB_OBJS = \
    $(OBJ_DIR)\gtdos$(HB_OBJ_EXT)    \

#**********************************************************

GTPCA_LIB_OBJS = \
    $(OBJ_DIR)\gtpca$(HB_OBJ_EXT)    \

#**********************************************************

GTSTD_LIB_OBJS = \
    $(OBJ_DIR)\gtstd$(HB_OBJ_EXT)    \

#**********************************************************

GTWIN_LIB_OBJS = \
    $(OBJ_DIR)\gtwin$(HB_OBJ_EXT)    \

#**********************************************************

GTWVT_LIB_OBJS = \
    $(OBJ_DIR)\gtwvt$(HB_OBJ_EXT)    \

#**********************************************************

GTGUI_LIB_COMMON_OBJS = \
    $(OBJ_DIR)\gtgui$(HB_OBJ_EXT)    \

GTGUI_LIB_STATIC_OBJS = \
    $(OBJ_DIR)\gtdef$(HB_OBJ_EXT)    \

GTGUI_LIB_SHARED_OBJS = \

GTGUI_LIB_OBJS = $(GTGUI_LIB_COMMON_OBJS) $(GTGUI_LIB_STATIC_OBJS)
GTGUI_DLL_OBJS = $(GTGUI_LIB_COMMON_OBJS) $(GTGUI_LIB_SHARED_OBJS)

#**********************************************************
#**********************************************************
#**********************************************************

HARBOUR_EXE_OBJS = \
    $(OBJ_DIR)\harbour$(HB_OBJ_EXT)  \

#**********************************************************

HBPP_EXE_OBJS = \
    $(OBJ_DIR)\hbpp$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\hbpptbl$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\hbppcomp$(HB_OBJ_EXT) \
    $(OBJ_DIR)\hbppcore$(HB_OBJ_EXT) \
    $(OBJ_DIR)\pragma$(HB_OBJ_EXT)   \

#    $(OBJ_DIR)\hbpplib$(HB_OBJ_EXT)  \

#**********************************************************

HBPPGEN_EXE_OBJS = \
    $(OBJ_DIR)\ppgen$(HB_OBJ_EXT)    \

#**********************************************************

HBPPTEST_EXE_OBJS = \
    $(OBJ_DIR)\pretest$(HB_OBJ_EXT)  \

#**********************************************************

HBRUN_EXE_OBJS = \
    $(OBJ_DIR)\hbrun$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\external$(HB_OBJ_EXT) \

#**********************************************************

HBTEST_EXE_OBJS = \
    $(OBJ_DIR)\hbtest$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\rt_hvm$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\rt_hvma$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\rt_math$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\rt_date$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\rt_str$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\rt_stra$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\rt_trans$(HB_OBJ_EXT) \
    $(OBJ_DIR)\rt_array$(HB_OBJ_EXT) \
    $(OBJ_DIR)\rt_file$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\rt_misc$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\rt_class$(HB_OBJ_EXT) \

#**********************************************************

HBDOC_EXE_OBJS = \
    $(OBJ_DIR)\hbdoc$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\genasc$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\genhpc$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\genhtm$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\genchm$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\genng$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\genos2$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\genrtf$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\gentrf$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\html$(HB_OBJ_EXT)     \
    $(OBJ_DIR)\ng$(HB_OBJ_EXT)       \
    $(OBJ_DIR)\os2$(HB_OBJ_EXT)      \
    $(OBJ_DIR)\rtf$(HB_OBJ_EXT)      \
    $(OBJ_DIR)\troff$(HB_OBJ_EXT)    \
    $(OBJ_DIR)\fclass1$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\ffile1$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\ft_funcs$(HB_OBJ_EXT) \

!ifdef HB_DOC_PDF

# PDF support for HBDOC
HBDOC_EXE_OBJS = \
    $(HBDOC_EXE_OBJS)       \
    $(OBJ_DIR)\pdfhbdoc$(HB_OBJ_EXT) \
    $(OBJ_DIR)\genpdf1$(HB_OBJ_EXT)  \

!endif

#**********************************************************

HBMAKE_EXE_OBJS = \
    $(OBJ_DIR)\hbmake$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\hbmutils$(HB_OBJ_EXT) \
    $(OBJ_DIR)\checks$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\pickarry$(HB_OBJ_EXT) \
    $(OBJ_DIR)\pickfile$(HB_OBJ_EXT) \
    $(OBJ_DIR)\prb_stak$(HB_OBJ_EXT) \
    $(OBJ_DIR)\radios$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\fclass1$(HB_OBJ_EXT)  \
    $(OBJ_DIR)\ffile1$(HB_OBJ_EXT)   \
    $(OBJ_DIR)\ft_funcs$(HB_OBJ_EXT) \
    $(OBJ_DIR)\hbmlang$(HB_OBJ_EXT)  \

#**********************************************************

HBVER_EXE_OBJS = \
    $(OBJ_DIR)\hbverfix$(HB_OBJ_EXT) \

#**********************************************************
#**********************************************************
#**********************************************************

#
# HARBOUR_DLL objects
#

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
    $(GTWIN_LIB_OBJS)       \
    $(GTWVT_LIB_OBJS)       \
    $(GTGUI_DLL_OBJS)       \

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
#**********************************************************
#**********************************************************
