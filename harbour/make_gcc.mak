#
# $Id$
#

#**********************************************************
# Makefile for Harbour Project for GNU gcc compiler
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
#                           dll in addition to normal static build (currently not working)
#       HB_BUILD_DEBUG    - If set to yes causes to compile with debug info
#       HB_BUILD_VERBOSE  - Controls echoing commands being executed
#       HB_BUILD_OPTIM    - Setting it to 'no' disables compiler optimizations
#       HB_REBUILD_PARSER - If set to yes force preprocessing new rules by
#                           Bison (you must use Bison 2.3 or later)
#       HB_INSTALL_PREFIX - Path to installation directory into which
#                           Harbour will be installed when using 'install'
#                           mode. Defaults to current directory
#
#       HB_GPM_MOUSE      - If set to yes enables using GPM mouse driver on console
#       HB_WITHOUT_GTSLN  - If set to yes causes to not build gtsln
#                           (if you don't have slang installed)
#       HB_COMMERCE       - If set to yes disables pure GNU modules (slang,GPM,...)

# ---------------------------------------------------------------

.SUFFIXES:

#**********************************************************

ifndef ECHO
ECHO = echo
endif
ifndef DEL
DEL = rm -f
endif
ifndef COPY
COPY = cp
endif

ifndef LIBPREF
LIBPREF=lib
endif

#**********************************************************

# CC and LD are set in make_gcc.sh

#ifeq ($(CC),)
#CC = gcc
#endif
#ifeq ($(LD),)
#LD = gcc
#endif
ifeq ($(MKLIB),)
MKLIB = ar
endif

#**********************************************************

# These defs have to be defined
# *before* common.cf is included

OBJEXT=.o
EXEEXT=$(if $(findstring $(HB_ARCHITECTURE),win dos os2),.exe)
DLLEXT=$(if $(findstring $(HB_ARCHITECTURE),win dos os2),.dll,.so)
LIBEXT=.a
LIBPREF=lib

#**********************************************************

# Include Common Object list files shared
# between Msvc, Borland and Gcc compilers

include common.cf

#**********************************************************

# Hack for WINDOWS systems. We're removing mainstd file
# from VM lib and we're putting it into a new library
# libmainstd.a to allow building either CONSOLE or GUI
# programs, depending on which library we're linking
# against. If we're linking against libmainstd we're
# building CONSOLE programs. Otherwise we're building
# GUI programs without console. Please note IT IS A
# DIRTY HACK and any better solution is HIGHLY WELCOME
ifneq ($(findstring $(HB_ARCHITECTURE),win os2),)
MAIN_LIB      = $(LIB_DIR)/$(LIBPREF)hbmainstd$(LIBEXT)
MAIN_LIB_OBJS = $(OBJ_DIR)/mainstd$(OBJEXT)

VM_STATIC_LIB_OBJS     := $(VM_STATIC_LIB_OBJS:$(OBJ_DIR)/mainstd$(OBJEXT)=)
STANDARD_STATIC_HBLIBS := $(STANDARD_STATIC_HBLIBS) $(MAIN_LIB)
HB_BUILD_TARGETS       := $(MAIN_LIB) $(HB_BUILD_TARGETS)
endif

#**********************************************************

#.SUFFIXES: $(EXEEXT) $(LIBEXT) $(OBJEXT) .prg .c .l .y

#**********************************************************

# Default sources directory search paths
VPATH := $(ALL_SRC_DIRS) $(LIB_DIR) $(BIN_DIR) $(OBJ_DIR) $(MT_OBJ_DIR) $(DLL_OBJ_DIR)

#**********************************************************

# Some definitions cannot be kept in common.mak
# due to serious limitations of Microsoft Nmake

VMMT_LIB_OBJS = $(subst $(OBJ_DIR),$(MT_OBJ_DIR),$(VM_LIB_OBJS))

# Do not perform an extra compilation phase for shared libraries
# if gcc -fPIC compilation flag is already passed to a makefile
ifneq ($(findstring -fPIC,$(HB_USER_CFLAGS)),)
DLL_OBJS = $(TMP_DLL_OBJS) $(VM_DLL_OBJS)
MTDLL_OBJS = $(TMP_DLL_OBJS) $(VM_DLL_OBJS:$(OBJ_DIR)/%=$(MT_OBJ_DIR)/%)
else
DLL_OBJS := $(patsubst $(OBJ_DIR)/%,$(DLL_OBJ_DIR)/%,$(TMP_DLL_OBJS) $(VM_DLL_OBJS))
MTDLL_OBJS := $(TMP_DLL_OBJS:$(OBJ_DIR)/%=$(DLL_OBJ_DIR)/%) $(VM_DLL_OBJS:$(OBJ_DIR)/%=$(MTDLL_OBJ_DIR)/%)
endif


# DLLs on Windows require IMPORT lib
# and an additional compiler phase
ifneq ($(findstring $(HB_ARCHITECTURE),win),)
HB_DLL_IMPLIB := $(HARBOUR_DLL:%$(DLLEXT)=%$(LIBEXT))
HB_IMPLIB_PART := -Wl,--out-implib,$(HB_DLL_IMPLIB)
HB_DLL_IMPLIBMT := $(HARBOURMT_DLL:%$(DLLEXT)=%$(LIBEXT))
HB_IMPLIBMT_PART := -Wl,--out-implib,$(HB_DLL_IMPLIBMT)
endif

#**********************************************************
# C compiler flags
#**********************************************************

CFLAGS         := -W -Wall -I$(INCLUDE_DIR) $(HB_USER_CFLAGS) -I$(OBJ_DIR)
CFLAGSMT       := -DHB_MT_VM
#-----------
ifneq ($(HB_BUILD_OPTIM),no)
ifeq ($(HB_ARCHITECTURE),win)
CFLAGS         := -march=i586 $(CFLAGS)
endif
CFLAGS         := -O3 $(CFLAGS)
endif
#-----------
ifeq ($(HB_BUILD_DEBUG),yes)
CFLAGS         := -g -DHB_TR_LEVEL_DEBUG $(CFLAGS)
endif
#-----------
CLIBFLAGS      := -c $(CFLAGS)
CLIBFLAGSDLL   := $(CLIBFLAGS) -DHB_DYNLIB
CEXEFLAGSDLL   := $(CFLAGS)

# Under architectures other than "DOS based" add -fPIC
# to gcc compiler flags for compiling shared libraries
ifeq ($(findstring $(HB_ARCHITECTURE),win os2),)
ifeq ($(findstring -fPIC,$(CLIBFLAGSDLL)),)
CLIBFLAGSDLL   := -fPIC $(CLIBFLAGSDLL)
endif
endif

#**********************************************************
# Linker Flags
#**********************************************************

# OS/2 hacks for missing gcc features
ifneq ($(HB_ARCHITECTURE),os2)
ifneq ($(HB_ARCHITECTURE),sunos)
ifneq ($(HB_ARCHITECTURE),hpux)
ifneq ($(HB_ARCHITECTURE),darwin)
__GROUP_LIBS_BEG__=-Wl,--start-group
__GROUP_LIBS_END__=-Wl,--end-group
endif
endif
endif
endif

LDFLAGS := $(HB_USER_LDFLAGS) $(__GROUP_LIBS_BEG__) $(STANDARD_STATIC_HBLIBS)

LDFLAGS += $(__GROUP_LIBS_END__)

ifeq ($(__GROUP_LIBS_BEG__),)
LDFLAGS += $(RTL_LIB) $(VM_LIB)
LDFLAGS += $(STANDARD_STATIC_HBLIBS)
endif

# HB_GPM_MOUSE: use gpm mouse driver
ifeq ($(HB_GPM_MOUSE),yes)
HB_OS_LIBS += -lgpm
CFLAGS  += -DHAVE_GPM_H
endif

# PCRE Regex library
ifneq ($(findstring -DHB_PCRE_REGEX, $(CFLAGS)),)
HB_OS_LIBS += -lpcre
endif

# ZLIB library
ifneq ($(findstring -DHB_EXT_ZLIB, $(CFLAGS)),)
HB_OS_LIBS += -lz
endif

LDFLAGS += $(HB_OS_LIBS)

LDFLAGSDLL := -shared $(HB_USER_LDFLAGS) -L$(LIB_DIR)

#**********************************************************
# Library manager Flags
#**********************************************************

ARFLAGS = rc $(HB_USER_AFLAGS)

#**********************************************************
# COMPILE Rules
#**********************************************************
# General *.prg --> *.o COMPILE rule for STATIC Libraries
$(OBJ_DIR)/%$(OBJEXT) : %.prg $(HARBOUR_EXE)
	$(HB) $(HARBOURFLAGSLIB) -o$(OBJ_DIR)/ $<
	$(CC) $(CLIBFLAGS) -o$@ $(OBJ_DIR)/$(<F:.prg=.c)
#----------------------------------------------------------
# General *.prg --> *.o COMPILE rule for STATIC MT Libraries
$(MT_OBJ_DIR)/%$(OBJEXT) : %.prg $(HARBOUR_EXE)
	$(HB) $(HARBOURFLAGSLIB) -o$(MT_OBJ_DIR)/ $<
	$(CC) $(CLIBFLAGS) $(CFLAGSMT) -o$@ $(MT_OBJ_DIR)/$(<F:.prg=.c)
#----------------------------------------------------------
# General *.c --> *.o COMPILE rule for STATIC Libraries
$(OBJ_DIR)/%$(OBJEXT) : %.c
	$(CC) $(CLIBFLAGS) -o$@ $<
#----------------------------------------------------------
# General *.c --> *.o COMPILE rule for STATIC MT Libraries
$(MT_OBJ_DIR)/%$(OBJEXT) : %.c
	$(CC) $(CLIBFLAGS) $(CFLAGSMT) -o$@ $<
#*******************************************************
# General *.prg --> *.o COMPILE rule for SHARED Libraries
$(DLL_OBJ_DIR)/%$(OBJEXT) : %.prg $(HARBOUR_EXE)
	$(HB) $(HARBOURFLAGSLIB) -o$(DLL_OBJ_DIR)/ $<
	$(CC) $(CLIBFLAGSDLL) -o$@ $(DLL_OBJ_DIR)/$(<F:.prg=.c)
#----------------------------------------------------------
# General *.prg --> *.o COMPILE rule for SHARED MT Libraries
$(MTDLL_OBJ_DIR)/%$(OBJEXT) : %.prg $(HARBOUR_EXE)
	$(HB) $(HARBOURFLAGSLIB) -o$(MTDLL_OBJ_DIR)/ $<
	$(CC) $(CLIBFLAGSDLL) $(CFLAGSMT) -o$@ $(MTDLL_OBJ_DIR)/$(<F:.prg=.c)
#----------------------------------------------------------
# General *.c --> *.o COMPILE rule for SHARED Libraries
$(DLL_OBJ_DIR)/%$(OBJEXT) : %.c
	$(CC) $(CLIBFLAGSDLL) -o$@ $<
#----------------------------------------------------------
# General *.c --> *.o COMPILE rule for SHARED MT Libraries
$(MTDLL_OBJ_DIR)/%$(OBJEXT) : %.c
	$(CC) $(CLIBFLAGSDLL) $(CFLAGSMT) -o$@ $<
#**********************************************************
# General *.o -> *.a LIBRARY CREATION rule
#$(LIB_DIR)/%$(LIBEXT) : %$(OBJEXT)
#	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************


#**********************************************************
# TARGET dependencies
#**********************************************************
all : $(HB_DEST_DIRS) $(HB_BUILD_TARGETS)
#**********************************************************


#**********************************************************
# Helper targets
#**********************************************************
.PHONY : BasicLibs BasicExes StdLibs MinLibs
BasicLibs : $(COMMON_LIB) $(HBPP_EXE) $(PP_LIB) $(COMPILER_LIB)
BasicExes : BasicLibs $(HARBOUR_EXE)
StdLibs   : BasicExes $(STANDARD_STATIC_HBLIBS)
MinLibs   : $(MINIMAL_STATIC_HBLIBS)
#**********************************************************
$(MAIN_DIR)/harbour.c : $(OBJ_DIR)/pptable.c
$(VM_DIR)/cmdarg.c : $(OBJ_DIR)/pptable.c
#**********************************************************


#**********************************************************
# Implicit directory creation rule
$(HB_DEST_DIRS) $(HB_BIN_INSTALL) $(HB_LIB_INSTALL) $(HB_INC_INSTALL):
	mkdir -p $@
#**********************************************************


#**********************************************************
# LIBRARY Targets BUILD rules
#**********************************************************
$(HBMAINSTD_LIB): $(HBMAINSTD_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(HBMAINWIN_LIB): $(HBMAINWIN_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(COMMON_LIB)   : $(COMMON_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(PP_LIB)       : $(PP_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(COMPILER_LIB) : $(COMPILER_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(VM_LIB)       : $(VM_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(VMMT_LIB)     : $(VMMT_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
ifneq ($(findstring $(HB_ARCHITECTURE),win os2),)
$(MAIN_LIB)     : $(MAIN_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
endif
#**********************************************************
$(RTL_LIB)      : $(RTL_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(MACRO_LIB)    : $(MACRO_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(DEBUG_LIB)    : $(DEBUG_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(LANG_LIB)     : $(LANG_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(CODEPAGE_LIB) : $(CODEPAGE_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(PCRE_LIB)     : $(PCRE_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(HBZLIB_LIB)   : $(HBZLIB_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(HBEXTERN_LIB) : $(HBEXTERN_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(RDD_LIB)      : $(RDD_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(NULSYS_LIB)   : $(NULSYS_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(DBFNTX_LIB)   : $(DBFNTX_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(DBFNSX_LIB)   : $(DBFNSX_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(DBFCDX_LIB)   : $(DBFCDX_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(DBFFPT_LIB)   : $(DBFFPT_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(HBSIX_LIB)    : $(HBSIX_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(HSX_LIB)      : $(HSX_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(USRRDD_LIB)   : $(USRRDD_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(HBUDDALL_LIB) : $(HBUDDALL_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(GTCGI_LIB)    : $(GTCGI_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(GTSTD_LIB)    : $(GTSTD_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(GTPCA_LIB)    : $(GTPCA_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(GTDOS_LIB)    : $(GTDOS_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(GTWIN_LIB)    : $(GTWIN_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(GTWVT_LIB)    : $(GTWVT_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(GTGUI_LIB)    : $(GTGUI_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(GTOS2_LIB)    : $(GTOS2_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(GTTRM_LIB)    : $(GTTRM_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(GTCRS_LIB)    : $(GTCRS_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(GTSLN_LIB)    : $(GTSLN_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************
$(GTXWC_LIB)    : $(GTXWC_LIB_OBJS)
	$(MKLIB) $(ARFLAGS) $@ $^
#**********************************************************


#**********************************************************
# EXECUTABLE Targets BUILD rules
#**********************************************************
$(HBPP_EXE)     : $(HBPP_EXE_OBJS) $(COMMON_LIB)
	$(CC) $(CFLAGS) -o $@ $^ $(HB_OS_LIBS)
#**********************************************************
$(HARBOUR_EXE)  : $(HARBOUR_EXE_OBJS) $(COMPILER_LIB) $(PP_LIB) $(COMMON_LIB)
	$(CC) $(CFLAGS) -o $@ $^ $(HB_OS_LIBS)
#**********************************************************
$(HBRUN_EXE)    :: StdLibs
$(HBRUN_EXE)    :: $(HBRUN_EXE_OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)
#**********************************************************
$(HBTEST_EXE)   :: StdLibs
$(HBTEST_EXE)   :: $(HBTEST_EXE_OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)
#**********************************************************
$(HBI18N_EXE)   :: StdLibs
$(HBI18N_EXE)   :: $(HBI18N_EXE_OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)
#**********************************************************
$(HBDOC_EXE)    :: StdLibs
$(HBDOC_EXE)    :: $(HBDOC_EXE_OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)
#**********************************************************
$(HBMK_EXE)     :: StdLibs
$(HBMK_EXE)     :: $(HBMK_EXE_OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)
#**********************************************************

#**********************************************************
# DLL Targets
#**********************************************************
$(HARBOUR_DLL) :: StdLibs
$(HARBOUR_DLL) :: $(DLL_OBJS)
	$(CC) $(LDFLAGSDLL) -o $@ $^ $(HB_OS_LIBS) $(HB_IMPLIB_PART)
#**********************************************************
$(HARBOURMT_DLL) :: StdLibs
$(HARBOURMT_DLL) :: $(MTDLL_OBJS)
	$(CC) $(LDFLAGSDLL) -o $@ $^ $(HB_OS_LIBS) $(HB_IMPLIBMT_PART)
#**********************************************************
# DLL EXECUTABLE Targets
#**********************************************************
$(HBTESTDLL_EXE) :: $(HARBOUR_DLL)
$(HBTESTDLL_EXE) :: $(OBJ_DIR)/mainstd$(OBJEXT) $(HBTEST_EXE_OBJS)
	$(CC) $(CEXEFLAGSDLL) -o$@ $^ -L$(BIN_DIR) -l$(HARBOUR_DLL:$(BIN_DIR)/lib%.so=%) $(HB_OS_LIBS)
#**********************************************************
$(HBRUNDLL_EXE) :: $(HARBOUR_DLL)
$(HBRUNDLL_EXE) :: $(OBJ_DIR)/mainstd$(OBJEXT) $(HBRUN_EXE_OBJS)
	$(CC) $(CEXEFLAGSDLL) -o$@ $^ -L$(BIN_DIR) -l$(HARBOUR_DLL:$(BIN_DIR)/lib%.so=%) $(COMPILER_LIB) $(HB_OS_LIBS)
#**********************************************************

#**********************************************************
# EXTRA Object's DEPENDENCIES
#**********************************************************

# Generated by an intermediate utility hbpp.exe
# built at the initial phase of build process
$(OBJ_DIR)/pptable$(OBJEXT) : $(OBJ_DIR)/pptable.c
$(DLL_OBJ_DIR)/pptable$(OBJEXT) : $(DLL_OBJ_DIR)/pptable.c

$(OBJ_DIR)/pptable.c     : $(HBPP) $(INCLUDE_DIR)/hbstdgen.ch $(INCLUDE_DIR)/std.ch ChangeLog $(PP_DIR)/ppcore.c $(PP_DIR)/hbpp.c
	$< $(INCLUDE_DIR)/hbstdgen.ch -o$@ -q -cChangeLog -v$(INCLUDE_DIR)/hbverbld.h

$(DLL_OBJ_DIR)/pptable.c : $(HBPP) $(INCLUDE_DIR)/hbstdgen.ch $(INCLUDE_DIR)/std.ch ChangeLog $(PP_DIR)/ppcore.c $(PP_DIR)/hbpp.c
	$< $(INCLUDE_DIR)/hbstdgen.ch -o$@ -q -cChangeLog -v$(INCLUDE_DIR)/hbverbld.h

#**********************************************************

# additional dependencies for parallel execution
$(OBJ_DIR)/macrolex$(OBJEXT) : $(OBJ_DIR)/macroy.c
$(OBJ_DIR)/complex$(OBJEXT) : $(OBJ_DIR)/harboury.c

ifeq ("$(HB_REBUILD_PARSER)","yes")

$(OBJ_DIR)/macroy.c : $(MACRO_DIR)/macro.y
	bison --no-line -d $< -o$@

$(OBJ_DIR)/harboury.c : $(COMPILER_DIR)/harbour.y
	bison --no-line -d $< -o$@

else

$(OBJ_DIR)/macroy.c : $(MACRO_DIR)/macro.yyc
	$(COPY) $< $@
	$(COPY) $(<:.yyc=.yyh) $(@:.c=.h)

$(OBJ_DIR)/harboury.c : $(COMPILER_DIR)/harbour.yyc
	$(COPY) $< $@
	$(COPY) $(<:.yyc=.yyh) $(@:.c=.h)

endif

#**********************************************************

#$(OBJ_DIR)/macrol.c : $(MACRO_DIR)/macro.l
#	flex -Phb_macro -i -8 -o$@ $<

#$(OBJ_DIR)/harbourl.c : $(COMPILER_DIR)/harbour.l
#	flex -Phb_comp -i -8 -o$@ $<

#$(OBJ_DIR)/harbourl$(OBJEXT) : $(OBJ_DIR)/harbourl.c
#$(OBJ_DIR)/macrol$(OBJEXT)   : $(OBJ_DIR)/macrol.c

#**********************************************************

# additional dependencies for parallel execution
$(DLL_OBJ_DIR)/macrolex$(OBJEXT) : $(DLL_OBJ_DIR)/macroy.c
$(DLL_OBJ_DIR)/complex$(OBJEXT) : $(DLL_OBJ_DIR)/harboury.c

ifeq ("$(HB_REBUILD_PARSER)","yes")

$(DLL_OBJ_DIR)/macroy.c : $(MACRO_DIR)/macro.y
	bison --no-line -d $< -o$@

$(DLL_OBJ_DIR)/harboury.c : $(COMPILER_DIR)/harbour.y
	bison --no-line -d $< -o$@

else

$(DLL_OBJ_DIR)/macroy.c : $(MACRO_DIR)/macro.yyc
	$(COPY) $< $@
	$(COPY) $(<:.yyc=.yyh) $(@:.c=.h)

$(DLL_OBJ_DIR)/harboury.c : $(COMPILER_DIR)/harbour.yyc
	$(COPY) $< $@
	$(COPY) $(<:.yyc=.yyh) $(@:.c=.h)

endif

#**********************************************************

#$(DLL_OBJ_DIR)/macrol.c : $(MACRO_DIR)/macro.l
#	flex -Phb_macro -i -8 -o$@ $<

#$(DLL_OBJ_DIR)/harbourl.c : $(COMPILER_DIR)/harbour.l
#	flex -Phb_comp -i -8 -o$@ $<

#$(DLL_OBJ_DIR)/harbourl$(OBJEXT) : $(DLL_OBJ_DIR)/harbourl.c
#$(DLL_OBJ_DIR)/macrol$(OBJEXT)   : $(DLL_OBJ_DIR)/macrol.c

#**********************************************************

$(DLL_OBJ_DIR)/harboury$(OBJEXT) : $(DLL_OBJ_DIR)/harboury.c
$(DLL_OBJ_DIR)/macroy$(OBJEXT)   : $(DLL_OBJ_DIR)/macroy.c

#**********************************************************
# CLEAN rules
#**********************************************************

clean: doClean
Clean: doClean
CLEAN: doClean

doClean:
	-$(DEL) $(HB_BUILD_TARGETS) $(HB_DLL_IMPLIB) $(HB_DLL_IMPLIBMT)
	-$(DEL) $(OBJ_DIR)/*$(OBJEXT)
	-$(DEL) $(OBJ_DIR)/*.c
	-$(DEL) $(OBJ_DIR)/*.h
	-$(DEL) $(DLL_OBJ_DIR)/*$(OBJEXT)
	-$(DEL) $(DLL_OBJ_DIR)/*.c
	-$(DEL) $(DLL_OBJ_DIR)/*.h
	-$(DEL) $(MT_OBJ_DIR)/*$(OBJEXT)
	-$(DEL) $(MT_OBJ_DIR)/*.c
	-$(DEL) $(MT_OBJ_DIR)/*.h
	-$(DEL) $(MTDLL_OBJ_DIR)/*$(OBJEXT)
	-$(DEL) $(MTDLL_OBJ_DIR)/*.c
	-$(DEL) $(MTDLL_OBJ_DIR)/*.h
	-$(DEL) $(INCLUDE_DIR)/hbverbld.h
	-$(DEL) inst_$(HB_CC_NAME).log
	-$(DEL) common.cf

#**********************************************************
# INSTALL rules
#**********************************************************

install : doInstall
Install : doInstall
INSTALL : doInstall

doInstall: $(HB_BIN_INSTALL) $(HB_LIB_INSTALL) $(HB_INC_INSTALL)
	-for n in $(HB_BUILD_TARGETS); \
	 do \
	   [ -f "$$n" ] && \
	   case $$n in \
	     *$(DLLEXT) ) $(COPY) $$n $(HB_BIN_INSTALL);; \
	     *$(LIBEXT) ) $(COPY) $$n $(HB_LIB_INSTALL);; \
	     *$(EXEEXT) ) $(COPY) $$n $(HB_BIN_INSTALL);; \
	   esac \
	 done
ifneq  ("$(HB_INSTALL_PREFIX)",".")
	-[ ! -d "$(HB_INC_INSTALL)" ] || $(COPY) $(INCLUDE_DIR)/*.api $(HB_INC_INSTALL)
	-[ ! -d "$(HB_INC_INSTALL)" ] || $(COPY) $(INCLUDE_DIR)/*.ch  $(HB_INC_INSTALL)
	-[ ! -d "$(HB_INC_INSTALL)" ] || $(COPY) $(INCLUDE_DIR)/*.h   $(HB_INC_INSTALL)
endif

#**********************************************************
