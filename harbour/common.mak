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

COMMON_DIR   = source\common
PP_DIR       = source\pp
VM_DIR       = source\vm
RTL_DIR      = source\rtl
MACRO_DIR    = source\macro
DEBUG_DIR    = source\debug
LANG_DIR     = source\lang
CODEPAGE_DIR = source\codepage
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
$(COMMON_DIR);\
$(PP_DIR);\
$(VM_DIR);\
$(RTL_DIR);\
$(MACRO_DIR);\
$(DEBUG_DIR);\
$(LANG_DIR);\
$(CODEPAGE_DIR);\
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

COMMON_LIB   = $(LIB_DIR)\common.lib
PP_LIB       = $(LIB_DIR)\pp.lib
VM_LIB       = $(LIB_DIR)\vm.lib
RTL_LIB      = $(LIB_DIR)\rtl.lib
MACRO_LIB    = $(LIB_DIR)\macro.lib
DEBUG_LIB    = $(LIB_DIR)\debug.lib
LANG_LIB     = $(LIB_DIR)\lang.lib
CODEPAGE_LIB = $(LIB_DIR)\codepage.lib
RDD_LIB      = $(LIB_DIR)\rdd.lib
NULSYS_LIB   = $(LIB_DIR)\nulsys.lib
DBFNTX_LIB   = $(LIB_DIR)\dbfntx.lib
DBFCDX_LIB   = $(LIB_DIR)\dbfcdx.lib
DBFFPT_LIB   = $(LIB_DIR)\dbffpt.lib
HBSIX_LIB    = $(LIB_DIR)\hbsix.lib
HSX_LIB      = $(LIB_DIR)\hsx.lib
USRRDD_LIB   = $(LIB_DIR)\usrrdd.lib

GTCGI_LIB    = $(LIB_DIR)\gtcgi.lib
GTDOS_LIB    = $(LIB_DIR)\gtdos.lib
GTPCA_LIB    = $(LIB_DIR)\gtpca.lib
GTSTD_LIB    = $(LIB_DIR)\gtstd.lib
GTWIN_LIB    = $(LIB_DIR)\gtwin.lib
GTWVT_LIB    = $(LIB_DIR)\gtwvt.lib
GTGUI_LIB    = $(LIB_DIR)\gtgui.lib

HARBOUR_EXE  = $(BIN_DIR)\harbour.exe
# required (intermediate) utility
#     to generate pptable.c
HBPPGEN_EXE  = $(BIN_DIR)\ppgen.exe
HBPP_EXE     = $(BIN_DIR)\hbpp.exe
HBPPTEST_EXE = $(BIN_DIR)\hbpptest.exe
HBRUN_EXE    = $(BIN_DIR)\hbrun.exe
HBTEST_EXE   = $(BIN_DIR)\hbtest.exe
HBDOC_EXE    = $(BIN_DIR)\hbdoc.exe
HBMAKE_EXE   = $(BIN_DIR)\hbmake.exe
HBVER_EXE    = $(BIN_DIR)\hbverfix.exe

HARBOUR_DLL  = $(BIN_DIR)\harbour-$(CC_DIRNAME).dll
HBTESTDLL_EXE= $(BIN_DIR)\hbtest-dll.exe

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
HB_GT_LIB = $(LIB_DIR)\$(HB_GT_LIB).lib
!endif

#**********************************************************
#**********************************************************
#**********************************************************

# Standard Libs for HB-based executables
STANDARD_STATIC_HBLIBS = \
    $(COMMON_LIB)     \
    $(PP_LIB)         \
    $(VM_LIB)         \
    $(RTL_LIB)        \
    $(HB_GT_LIB)      \
    $(LANG_LIB)       \
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
    $(OBJ_DIR)\expropt1.obj \
    $(OBJ_DIR)\expropt2.obj \
    $(OBJ_DIR)\hbarch.obj   \
    $(OBJ_DIR)\hbfhnd.obj   \
    $(OBJ_DIR)\hbfsapi.obj  \
    $(OBJ_DIR)\hbgete.obj   \
    $(OBJ_DIR)\hbhash.obj   \
    $(OBJ_DIR)\hbdate.obj   \
    $(OBJ_DIR)\hbstr.obj    \
    $(OBJ_DIR)\hbtrace.obj  \
    $(OBJ_DIR)\hbver.obj    \
    $(OBJ_DIR)\hbverdsp.obj \
    $(OBJ_DIR)\reserved.obj

#**********************************************************

PP_LIB_OBJS = \
    $(OBJ_DIR)\pptable.obj \
    $(OBJ_DIR)\ppcore.obj  \
    $(OBJ_DIR)\pplib.obj   \
    $(OBJ_DIR)\pplib2.obj   \
    $(OBJ_DIR)\pplib3.obj   \

#**********************************************************

# VM Objects common for STATIC and SHARED library
VM_COMMON_LIB_OBJS = \
    $(OBJ_DIR)\arrays.obj   \
    $(OBJ_DIR)\arrayshb.obj \
    $(OBJ_DIR)\asort.obj    \
    $(OBJ_DIR)\break.obj    \
    $(OBJ_DIR)\classes.obj  \
    $(OBJ_DIR)\cmdarg.obj   \
    $(OBJ_DIR)\codebloc.obj \
    $(OBJ_DIR)\debug.obj    \
    $(OBJ_DIR)\dynlibhb.obj \
    $(OBJ_DIR)\dynsym.obj   \
    $(OBJ_DIR)\estack.obj   \
    $(OBJ_DIR)\eval.obj     \
    $(OBJ_DIR)\evalhb.obj   \
    $(OBJ_DIR)\extend.obj   \
    $(OBJ_DIR)\fm.obj       \
    $(OBJ_DIR)\garbage.obj  \
    $(OBJ_DIR)\hvm.obj      \
    $(OBJ_DIR)\initexit.obj \
    $(OBJ_DIR)\initsymb.obj \
    $(OBJ_DIR)\itemapi.obj  \
    $(OBJ_DIR)\macro.obj    \
    $(OBJ_DIR)\memvars.obj  \
    $(OBJ_DIR)\memvclip.obj \
    $(OBJ_DIR)\pcount.obj   \
    $(OBJ_DIR)\proc.obj     \
    $(OBJ_DIR)\pvalue.obj   \
    $(OBJ_DIR)\runner.obj   \
    $(OBJ_DIR)\harbinit.obj \

# Specific VM Objects for building STATIC library
VM_STATIC_LIB_OBJS = \
    $(OBJ_DIR)\mainstd.obj  \
    $(OBJ_DIR)\mainwin.obj

# Specific VM Objects for building SHARED (DLL) library
VM_SHARED_LIB_OBJS = \
    $(OBJ_DIR)\maindllh.obj \

# All VM Objects for building STATIC library
VM_LIB_OBJS = $(VM_COMMON_LIB_OBJS) $(VM_STATIC_LIB_OBJS)

# All VM Objects for building SHARED (DLL) library
VM_DLL_OBJS = $(VM_COMMON_LIB_OBJS) $(VM_SHARED_LIB_OBJS)

DISABLED_VM_OBJS = \
    $(OBJ_DIR)\maindllp.obj \
    $(OBJ_DIR)\maindll.obj  \

#**********************************************************

RTL_LIB_OBJS = \
    $(OBJ_DIR)\abs.obj      \
    $(OBJ_DIR)\accept.obj   \
    $(OBJ_DIR)\ampm.obj     \
    $(OBJ_DIR)\at.obj       \
    $(OBJ_DIR)\binnum.obj   \
    $(OBJ_DIR)\binnumx.obj  \
    $(OBJ_DIR)\box.obj      \
    $(OBJ_DIR)\cdpapi.obj   \
    $(OBJ_DIR)\chrasc.obj   \
    $(OBJ_DIR)\colorind.obj \
    $(OBJ_DIR)\console.obj  \
    $(OBJ_DIR)\copyfile.obj \
    $(OBJ_DIR)\datec.obj    \
    $(OBJ_DIR)\dates.obj    \
    $(OBJ_DIR)\dateshb.obj  \
    $(OBJ_DIR)\datesx.obj   \
    $(OBJ_DIR)\defpath.obj  \
    $(OBJ_DIR)\descend.obj  \
    $(OBJ_DIR)\dirdrive.obj \
    $(OBJ_DIR)\direct.obj   \
    $(OBJ_DIR)\diskspac.obj \
    $(OBJ_DIR)\disksphb.obj \
    $(OBJ_DIR)\do.obj       \
    $(OBJ_DIR)\empty.obj    \
    $(OBJ_DIR)\errorapi.obj \
    $(OBJ_DIR)\errorint.obj \
    $(OBJ_DIR)\file.obj     \
    $(OBJ_DIR)\filehb.obj   \
    $(OBJ_DIR)\filesys.obj  \
    $(OBJ_DIR)\fkmax.obj    \
    $(OBJ_DIR)\fnsplit.obj  \
    $(OBJ_DIR)\fserror.obj  \
    $(OBJ_DIR)\fssize.obj   \
    $(OBJ_DIR)\fstemp.obj   \
    $(OBJ_DIR)\gete.obj     \
    $(OBJ_DIR)\gt.obj       \
    $(OBJ_DIR)\gtapi.obj    \
    $(OBJ_DIR)\gtapiu.obj   \
    $(OBJ_DIR)\gtclip.obj   \
    $(OBJ_DIR)\gtfunc.obj   \
    $(OBJ_DIR)\gtsys.obj    \
    $(OBJ_DIR)\gttone.obj   \
    $(OBJ_DIR)\gx.obj       \
    $(OBJ_DIR)\hardcr.obj   \
    $(OBJ_DIR)\hbffind.obj  \
    $(OBJ_DIR)\hbgtcore.obj \
    $(OBJ_DIR)\hbrandom.obj \
    $(OBJ_DIR)\idle.obj     \
    $(OBJ_DIR)\inkey.obj    \
    $(OBJ_DIR)\is.obj       \
    $(OBJ_DIR)\isprint.obj  \
    $(OBJ_DIR)\langapi.obj  \
    $(OBJ_DIR)\left.obj     \
    $(OBJ_DIR)\len.obj      \
    $(OBJ_DIR)\lennum.obj   \
    $(OBJ_DIR)\math.obj     \
    $(OBJ_DIR)\maxrow.obj   \
    $(OBJ_DIR)\memofile.obj \
    $(OBJ_DIR)\memoline.obj \
    $(OBJ_DIR)\minmax.obj   \
    $(OBJ_DIR)\mlcount.obj  \
    $(OBJ_DIR)\mlpos.obj    \
    $(OBJ_DIR)\mlctopos.obj \
    $(OBJ_DIR)\mpostolc.obj \
    $(OBJ_DIR)\mod.obj      \
    $(OBJ_DIR)\mouseapi.obj \
    $(OBJ_DIR)\mousex.obj   \
    $(OBJ_DIR)\mtran.obj    \
    $(OBJ_DIR)\natmsg.obj   \
    $(OBJ_DIR)\net.obj      \
    $(OBJ_DIR)\oemansi.obj  \
    $(OBJ_DIR)\oldbox.obj   \
    $(OBJ_DIR)\oldclear.obj \
    $(OBJ_DIR)\pad.obj      \
    $(OBJ_DIR)\padc.obj     \
    $(OBJ_DIR)\padl.obj     \
    $(OBJ_DIR)\padr.obj     \
    $(OBJ_DIR)\philes.obj   \
    $(OBJ_DIR)\philes53.obj \
    $(OBJ_DIR)\philesx.obj  \
    $(OBJ_DIR)\rat.obj      \
    $(OBJ_DIR)\replic.obj   \
    $(OBJ_DIR)\right.obj    \
    $(OBJ_DIR)\round.obj    \
    $(OBJ_DIR)\run.obj      \
    $(OBJ_DIR)\samples.obj  \
    $(OBJ_DIR)\saverest.obj \
    $(OBJ_DIR)\scroll.obj   \
    $(OBJ_DIR)\seconds.obj  \
    $(OBJ_DIR)\set.obj      \
    $(OBJ_DIR)\setcolor.obj \
    $(OBJ_DIR)\setcurs.obj  \
    $(OBJ_DIR)\setkey.obj   \
    $(OBJ_DIR)\setpos.obj   \
    $(OBJ_DIR)\setposbs.obj \
    $(OBJ_DIR)\shadow.obj   \
    $(OBJ_DIR)\soundex.obj  \
    $(OBJ_DIR)\space.obj    \
    $(OBJ_DIR)\spfiles.obj  \
    $(OBJ_DIR)\str.obj      \
    $(OBJ_DIR)\strpeek.obj  \
    $(OBJ_DIR)\strcase.obj  \
    $(OBJ_DIR)\strings.obj  \
    $(OBJ_DIR)\strmatch.obj \
    $(OBJ_DIR)\strtran.obj  \
    $(OBJ_DIR)\strzero.obj  \
    $(OBJ_DIR)\stuff.obj    \
    $(OBJ_DIR)\substr.obj   \
    $(OBJ_DIR)\teditorl.obj \
    $(OBJ_DIR)\tone.obj     \
    $(OBJ_DIR)\trace.obj    \
    $(OBJ_DIR)\transfrm.obj \
    $(OBJ_DIR)\trim.obj     \
    $(OBJ_DIR)\type.obj     \
    $(OBJ_DIR)\val.obj      \
    $(OBJ_DIR)\valtostr.obj \
    $(OBJ_DIR)\valtype.obj  \
    $(OBJ_DIR)\version.obj  \
    $(OBJ_DIR)\word.obj     \
    $(OBJ_DIR)\xhelp.obj    \
    $(OBJ_DIR)\xsavescr.obj \
    \
    $(OBJ_DIR)\achoice.obj  \
    $(OBJ_DIR)\adir.obj     \
    $(OBJ_DIR)\alert.obj    \
    $(OBJ_DIR)\altd.obj     \
    $(OBJ_DIR)\array.obj    \
    $(OBJ_DIR)\block.obj    \
    $(OBJ_DIR)\browdb.obj   \
    $(OBJ_DIR)\browdbx.obj  \
    $(OBJ_DIR)\browse.obj   \
    $(OBJ_DIR)\characte.obj \
    $(OBJ_DIR)\checkbox.obj \
    $(OBJ_DIR)\color53.obj  \
    $(OBJ_DIR)\date.obj     \
    $(OBJ_DIR)\dbedit.obj   \
    $(OBJ_DIR)\devoutp.obj  \
    $(OBJ_DIR)\dircmd.obj   \
    $(OBJ_DIR)\errorsys.obj \
    $(OBJ_DIR)\fieldbl.obj  \
    $(OBJ_DIR)\getlist.obj  \
    $(OBJ_DIR)\getsys.obj   \
    $(OBJ_DIR)\input.obj    \
    $(OBJ_DIR)\listbox.obj  \
    $(OBJ_DIR)\logical.obj  \
    $(OBJ_DIR)\memoedit.obj \
    $(OBJ_DIR)\memvarbl.obj \
    $(OBJ_DIR)\menuto.obj   \
    $(OBJ_DIR)\mssgline.obj \
    $(OBJ_DIR)\nil.obj      \
    $(OBJ_DIR)\numeric.obj  \
    $(OBJ_DIR)\objfunc.obj  \
    $(OBJ_DIR)\perfuncs.obj \
    $(OBJ_DIR)\persist.obj  \
    $(OBJ_DIR)\profiler.obj \
    $(OBJ_DIR)\pushbtn.obj  \
    $(OBJ_DIR)\radiobtn.obj \
    $(OBJ_DIR)\radiogrp.obj \
    $(OBJ_DIR)\readkey.obj  \
    $(OBJ_DIR)\readvar.obj  \
    $(OBJ_DIR)\scalar.obj   \
    $(OBJ_DIR)\scrollbr.obj \
    $(OBJ_DIR)\setfunc.obj  \
    $(OBJ_DIR)\setta.obj    \
    $(OBJ_DIR)\symbol.obj   \
    $(OBJ_DIR)\tbcolumn.obj \
    $(OBJ_DIR)\tbrowse.obj  \
    $(OBJ_DIR)\tclass.obj   \
    $(OBJ_DIR)\teditor.obj  \
    $(OBJ_DIR)\terror.obj   \
    $(OBJ_DIR)\text.obj     \
    $(OBJ_DIR)\tget.obj     \
    $(OBJ_DIR)\tgetint.obj  \
    $(OBJ_DIR)\tgetlist.obj \
    $(OBJ_DIR)\tlabel.obj   \
    $(OBJ_DIR)\tmenuitm.obj \
    $(OBJ_DIR)\tobject.obj  \
    $(OBJ_DIR)\tpopup.obj   \
    $(OBJ_DIR)\treport.obj  \
    $(OBJ_DIR)\ttextlin.obj \
    $(OBJ_DIR)\ttopbar.obj  \
    $(OBJ_DIR)\typefile.obj \
    $(OBJ_DIR)\wait.obj     \

#**********************************************************

MACRO_LIB_OBJS = \
    $(OBJ_DIR)\macroy.obj   \
    $(OBJ_DIR)\macroa.obj   \
    $(OBJ_DIR)\macrob.obj   \
    $(OBJ_DIR)\macroc.obj   \
    $(OBJ_DIR)\macrolex.obj \

#**********************************************************

DEBUG_LIB_OBJS = \
    $(OBJ_DIR)\dbgbrwsr.obj \
    $(OBJ_DIR)\dbghelp.obj  \
    $(OBJ_DIR)\dbgmenu.obj  \
    $(OBJ_DIR)\dbgtmenu.obj \
    $(OBJ_DIR)\dbgtmitm.obj \
    $(OBJ_DIR)\dbgtwin.obj  \
    $(OBJ_DIR)\debugger.obj \
    $(OBJ_DIR)\dbgtarr.obj  \
    $(OBJ_DIR)\dbgtobj.obj  \
    $(OBJ_DIR)\tbrwtext.obj \
    $(OBJ_DIR)\dbgwa.obj    \

#**********************************************************

LANG_LIB_OBJS = \
    $(OBJ_DIR)\msgbg866.obj \
    $(OBJ_DIR)\msgbgiso.obj \
    $(OBJ_DIR)\msgbgwin.obj \
    $(OBJ_DIR)\msgca.obj    \
    $(OBJ_DIR)\msgcs852.obj \
    $(OBJ_DIR)\msgcsiso.obj \
    $(OBJ_DIR)\msgcskam.obj \
    $(OBJ_DIR)\msgcswin.obj \
    $(OBJ_DIR)\msgde.obj    \
    $(OBJ_DIR)\msgdewin.obj \
    $(OBJ_DIR)\msgel.obj    \
    $(OBJ_DIR)\msgelwin.obj \
    $(OBJ_DIR)\msgeo.obj    \
    $(OBJ_DIR)\msges.obj    \
    $(OBJ_DIR)\msgeswin.obj \
    $(OBJ_DIR)\msgeu.obj    \
    $(OBJ_DIR)\msgfr.obj    \
    $(OBJ_DIR)\msggl.obj    \
    $(OBJ_DIR)\msghe862.obj \
    $(OBJ_DIR)\msghewin.obj \
    $(OBJ_DIR)\msghr852.obj \
    $(OBJ_DIR)\msghriso.obj \
    $(OBJ_DIR)\msghu852.obj \
    $(OBJ_DIR)\msghucwi.obj \
    $(OBJ_DIR)\msghuiso.obj \
    $(OBJ_DIR)\msghuwin.obj \
    $(OBJ_DIR)\msgid.obj    \
    $(OBJ_DIR)\msgis850.obj \
    $(OBJ_DIR)\msgit.obj    \
    $(OBJ_DIR)\msgko.obj    \
    $(OBJ_DIR)\msgpl852.obj \
    $(OBJ_DIR)\msgpliso.obj \
    $(OBJ_DIR)\msgplmaz.obj \
    $(OBJ_DIR)\msgplwin.obj \
    $(OBJ_DIR)\msgpt.obj    \
    $(OBJ_DIR)\msgro.obj    \
    $(OBJ_DIR)\msgru866.obj \
    $(OBJ_DIR)\msgrukoi.obj \
    $(OBJ_DIR)\msgruwin.obj \
    $(OBJ_DIR)\msgsl437.obj \
    $(OBJ_DIR)\msgsl852.obj \
    $(OBJ_DIR)\msgsliso.obj \
    $(OBJ_DIR)\msgslwin.obj \
    $(OBJ_DIR)\msgsr852.obj \
    $(OBJ_DIR)\msgsriso.obj \
    $(OBJ_DIR)\msgsrwin.obj \
    $(OBJ_DIR)\msgzhb5.obj  \
    $(OBJ_DIR)\msgzhgb.obj  \

#**********************************************************

CODEPAGE_LIB_OBJS = \
    $(OBJ_DIR)\cpbg866.obj  \
    $(OBJ_DIR)\cpbgiso.obj  \
    $(OBJ_DIR)\cpbgwin.obj  \
    $(OBJ_DIR)\cpcs852.obj  \
    $(OBJ_DIR)\cpcsiso.obj  \
    $(OBJ_DIR)\cpcskam.obj  \
    $(OBJ_DIR)\cpcswin.obj  \
    $(OBJ_DIR)\cpeldos.obj  \
    $(OBJ_DIR)\cpelwin.obj  \
    $(OBJ_DIR)\cpesdos.obj  \
    $(OBJ_DIR)\cpesmwi.obj  \
    $(OBJ_DIR)\cpeswin.obj  \
    $(OBJ_DIR)\cpfrdos.obj  \
    $(OBJ_DIR)\cpgedos.obj  \
    $(OBJ_DIR)\cpgewin.obj  \
    $(OBJ_DIR)\cphr1250.obj \
    $(OBJ_DIR)\cphr437.obj  \
    $(OBJ_DIR)\cphr852.obj  \
    $(OBJ_DIR)\cphu852.obj  \
    $(OBJ_DIR)\cphuiso.obj  \
    $(OBJ_DIR)\cphuwin.obj  \
    $(OBJ_DIR)\cpit437.obj  \
    $(OBJ_DIR)\cpit850.obj  \
    $(OBJ_DIR)\cpitisb.obj  \
    $(OBJ_DIR)\cpitiso.obj  \
    $(OBJ_DIR)\cpltwin.obj  \
    $(OBJ_DIR)\cppl852.obj  \
    $(OBJ_DIR)\cppliso.obj  \
    $(OBJ_DIR)\cpplmaz.obj  \
    $(OBJ_DIR)\cpplwin.obj  \
    $(OBJ_DIR)\cppt850.obj  \
    $(OBJ_DIR)\cpptiso.obj  \
    $(OBJ_DIR)\cpru866.obj  \
    $(OBJ_DIR)\cprukoi.obj  \
    $(OBJ_DIR)\cpruwin.obj  \
    $(OBJ_DIR)\cpsl437.obj  \
    $(OBJ_DIR)\cpsl852.obj  \
    $(OBJ_DIR)\cpsliso.obj  \
    $(OBJ_DIR)\cpslwin.obj  \
    $(OBJ_DIR)\cpsrwin.obj  \
    $(OBJ_DIR)\cpua866.obj  \
    $(OBJ_DIR)\cpuakoi.obj  \
    $(OBJ_DIR)\cpuawin.obj  \
    $(OBJ_DIR)\uc1250.obj   \
    $(OBJ_DIR)\uc1251.obj   \
    $(OBJ_DIR)\uc1253.obj   \
    $(OBJ_DIR)\uc1257.obj   \
    $(OBJ_DIR)\uc737.obj    \
    $(OBJ_DIR)\uc850.obj    \
    $(OBJ_DIR)\uc852.obj    \
    $(OBJ_DIR)\uc866.obj    \
    $(OBJ_DIR)\uc8859_1.obj \
    $(OBJ_DIR)\uc8859_2.obj \
    $(OBJ_DIR)\uc8859_5.obj \
    $(OBJ_DIR)\uc88591b.obj \
    $(OBJ_DIR)\uckoi8.obj   \
    $(OBJ_DIR)\uckoi8u.obj  \
    $(OBJ_DIR)\ucmaz.obj    \
    $(OBJ_DIR)\uckam.obj    \

#**********************************************************

RDD_LIB_OBJS = \
    $(OBJ_DIR)\dbcmd.obj    \
    $(OBJ_DIR)\workarea.obj \
    $(OBJ_DIR)\dbf1.obj     \
    $(OBJ_DIR)\dbnubs.obj   \
    $(OBJ_DIR)\delim1.obj   \
    $(OBJ_DIR)\sdf1.obj     \
    \
    $(OBJ_DIR)\dbdelim.obj  \
    $(OBJ_DIR)\dbsdf.obj    \
    $(OBJ_DIR)\dbjoin.obj   \
    $(OBJ_DIR)\dbtotal.obj  \
    $(OBJ_DIR)\dbfuncs.obj  \
    $(OBJ_DIR)\dblist.obj   \
    $(OBJ_DIR)\dbsort.obj   \
    $(OBJ_DIR)\hbdbsort.obj \
    $(OBJ_DIR)\dbstrux.obj  \
    $(OBJ_DIR)\dbupdat.obj  \
    $(OBJ_DIR)\rddord.obj   \
    $(OBJ_DIR)\rddsys.obj   \

#**********************************************************

NULSYS_LIB_OBJS = \
    $(OBJ_DIR)\nulsys.obj

#**********************************************************

DBFNTX_LIB_OBJS = \
    $(OBJ_DIR)\dbfntx1.obj  \
    $(OBJ_DIR)\dbfntx0.obj  \

#**********************************************************

DBFCDX_LIB_OBJS = \
    $(OBJ_DIR)\dbfcdx1.obj  \
    $(OBJ_DIR)\sixcdx1.obj  \

#**********************************************************

DBFFPT_LIB_OBJS = \
    $(OBJ_DIR)\dbffpt1.obj  \

#**********************************************************

HBSIX_LIB_OBJS = \
    $(OBJ_DIR)\sxcompr.obj  \
    $(OBJ_DIR)\sxcrypt.obj  \
    $(OBJ_DIR)\sxdate.obj   \

#**********************************************************

HSX_LIB_OBJS = \
    $(OBJ_DIR)\hsx.obj      \
    $(OBJ_DIR)\cftsfunc.obj \

#**********************************************************

USRRDD_LIB_OBJS = \
    $(OBJ_DIR)\usrrdd.obj   \

#**********************************************************

GTCGI_LIB_OBJS = \
    $(OBJ_DIR)\gtcgi.obj    \

#**********************************************************

GTDOS_LIB_OBJS = \
    $(OBJ_DIR)\gtdos.obj    \

#**********************************************************

GTPCA_LIB_OBJS = \
    $(OBJ_DIR)\gtpca.obj    \

#**********************************************************

GTSTD_LIB_OBJS = \
    $(OBJ_DIR)\gtstd.obj    \

#**********************************************************

GTWIN_LIB_OBJS = \
    $(OBJ_DIR)\gtwin.obj    \

#**********************************************************

GTWVT_LIB_OBJS = \
    $(OBJ_DIR)\gtwvt.obj    \

#**********************************************************

GTGUI_LIB_COMMON_OBJS = \
    $(OBJ_DIR)\gtgui.obj    \

GTGUI_LIB_STATIC_OBJS = \
    $(OBJ_DIR)\gtdef.obj    \

GTGUI_LIB_SHARED_OBJS = \

GTGUI_LIB_OBJS = $(GTGUI_LIB_COMMON_OBJS) $(GTGUI_LIB_STATIC_OBJS)
GTGUI_DLL_OBJS = $(GTGUI_LIB_COMMON_OBJS) $(GTGUI_LIB_SHARED_OBJS)

#**********************************************************
#**********************************************************
#**********************************************************

HARBOUR_EXE_OBJS = \
    $(OBJ_DIR)\harbour.obj  \
    $(OBJ_DIR)\harboury.obj \
    $(OBJ_DIR)\complex.obj  \
    $(OBJ_DIR)\cmdcheck.obj \
    $(OBJ_DIR)\hbcomp.obj   \
    $(OBJ_DIR)\hbdead.obj   \
    $(OBJ_DIR)\hbstripl.obj \
    $(OBJ_DIR)\hbusage.obj  \
    $(OBJ_DIR)\hbident.obj  \
    $(OBJ_DIR)\hbgenerr.obj \
    $(OBJ_DIR)\hblbl.obj    \
    $(OBJ_DIR)\hbpcode.obj  \
    $(OBJ_DIR)\hbfunchk.obj \
    $(OBJ_DIR)\hbfix.obj    \
    $(OBJ_DIR)\ppcomp.obj   \
    $(OBJ_DIR)\genc.obj     \
    $(OBJ_DIR)\gencc.obj    \
    $(OBJ_DIR)\gencli.obj   \
    $(OBJ_DIR)\gencobj.obj  \
    $(OBJ_DIR)\genobj32.obj \
    $(OBJ_DIR)\genjava.obj  \
    $(OBJ_DIR)\genhrb.obj   \
    $(OBJ_DIR)\expropta.obj \
    $(OBJ_DIR)\exproptb.obj \
    $(OBJ_DIR)\exproptc.obj \

#**********************************************************

HBPP_EXE_OBJS = \
    $(OBJ_DIR)\hbpp.obj     \
    $(OBJ_DIR)\hbpptbl.obj  \
    $(OBJ_DIR)\hbppcomp.obj \
    $(OBJ_DIR)\hbppcore.obj \
    $(OBJ_DIR)\pragma.obj   \

#    $(OBJ_DIR)\hbpplib.obj  \

#**********************************************************

HBPPGEN_EXE_OBJS = \
    $(OBJ_DIR)\ppgen.obj    \

#**********************************************************

HBPPTEST_EXE_OBJS = \
    $(OBJ_DIR)\pretest.obj  \

#**********************************************************

HBRUN_EXE_OBJS = \
    $(OBJ_DIR)\hbrun.obj    \
    $(OBJ_DIR)\external.obj \

#**********************************************************

HBTEST_EXE_OBJS = \
    $(OBJ_DIR)\hbtest.obj   \
    $(OBJ_DIR)\rt_hvm.obj   \
    $(OBJ_DIR)\rt_hvma.obj  \
    $(OBJ_DIR)\rt_math.obj  \
    $(OBJ_DIR)\rt_date.obj  \
    $(OBJ_DIR)\rt_str.obj   \
    $(OBJ_DIR)\rt_stra.obj  \
    $(OBJ_DIR)\rt_trans.obj \
    $(OBJ_DIR)\rt_array.obj \
    $(OBJ_DIR)\rt_file.obj  \
    $(OBJ_DIR)\rt_misc.obj  \
    $(OBJ_DIR)\rt_class.obj \

#**********************************************************

HBDOC_EXE_OBJS = \
    $(OBJ_DIR)\hbdoc.obj    \
    $(OBJ_DIR)\genasc.obj   \
    $(OBJ_DIR)\genhpc.obj   \
    $(OBJ_DIR)\genhtm.obj   \
    $(OBJ_DIR)\genchm.obj   \
    $(OBJ_DIR)\genng.obj    \
    $(OBJ_DIR)\genos2.obj   \
    $(OBJ_DIR)\genrtf.obj   \
    $(OBJ_DIR)\gentrf.obj   \
    $(OBJ_DIR)\html.obj     \
    $(OBJ_DIR)\ng.obj       \
    $(OBJ_DIR)\os2.obj      \
    $(OBJ_DIR)\rtf.obj      \
    $(OBJ_DIR)\troff.obj    \
    $(OBJ_DIR)\fclass1.obj  \
    $(OBJ_DIR)\ffile1.obj   \
    $(OBJ_DIR)\ft_funcs.obj \

!ifdef HB_DOC_PDF

# PDF support for HBDOC
HBDOC_EXE_OBJS = \
    $(HBDOC_EXE_OBJS)       \
    $(OBJ_DIR)\pdfhbdoc.obj \
    $(OBJ_DIR)\genpdf1.obj  \

!endif

#**********************************************************

HBMAKE_EXE_OBJS = \
    $(OBJ_DIR)\hbmake.obj   \
    $(OBJ_DIR)\hbmutils.obj \
    $(OBJ_DIR)\checks.obj   \
    $(OBJ_DIR)\pickarry.obj \
    $(OBJ_DIR)\pickfile.obj \
    $(OBJ_DIR)\prb_stak.obj \
    $(OBJ_DIR)\radios.obj   \
    $(OBJ_DIR)\fclass1.obj  \
    $(OBJ_DIR)\ffile1.obj   \
    $(OBJ_DIR)\ft_funcs.obj \
    $(OBJ_DIR)\hbmlang.obj  \

#**********************************************************

HBVER_EXE_OBJS = \
    $(OBJ_DIR)\hbverfix.obj \

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
    $(HARBOUR_EXE)          \
    $(HBPP_EXE)             \
    \
    $(VM_LIB)               \
    $(RTL_LIB)              \
    $(MACRO_LIB)            \
    $(DEBUG_LIB)            \
    $(LANG_LIB)             \
    $(CODEPAGE_LIB)         \
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
