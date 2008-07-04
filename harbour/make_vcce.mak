#
# $Id$
#

#**********************************************************
# Makefile for Harbour Project for MSVC-8.0/WinCE compilers
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
#       HB_GT_LIB         - To override the default GT driver
#                           (search for HB_GT_LIBS for a list of values)
#       HB_BUILD_ST       - If set to yes builds harbour in SingleThread mode
#       HB_BUILD_DLL      - If set to yes enables building harbour VM+RTL
#                           dll in addition to normal static build
#       HB_BUILD_MODE     - If set to cpp causes to compile in C++ mode
#       HB_BUILD_DEBUG    - If set to yes causes to compile with debug info
#       HB_BUILD_VERBOSE  - enables echoing commands being executed
#       HB_INSTALL_PREFIX - Path to instalation directory into which
#                           Harbour will be installed when the command
#                           "make_vc.bat install" is lauched. Defaults
#                           to current directory
#       HB_VISUALC_VER    - Version of Visual C++ compiler (defaults to 80).
#                           Possible values are : 60, 70, 71, 80

#**********************************************************

.SUFFIXES:

#**********************************************************

HB_ARCHITECTURE = w32

#**********************************************************

!if "$(HB_GT_LIB)" == ""
HB_GT_LIB = gtwvt
!endif

!if "$(HB_GT_DEFAULT)" == ""
HB_GT_DEFAULT = wvt
!endif

#**********************************************************

# Visual C++ version
!ifndef HB_VISUALC_VER
HB_VISUALC_VER = 80
!endif

#**********************************************************

!if $(HB_VISUALC_VER) >= 80
CC     = cl.exe
!else
CC     = clarm.exe
!endif
LINKER = link.exe
MKLIB  = lib.exe

#**********************************************************

# Include Common Object list files
# shared between MSVC and Borland

!include common.mak

#**********************************************************

.SUFFIXES: $(EXEEXT) $(LIBEXT) $(OBJEXT) .prg .c .l .y

#**********************************************************

# Some definitions cannot be kept in Common.mak
# due to serious limitations of Microsoft Nmake

# Nmake does not support macros in string
# substitution, so we have to hardcode it

DLL_OBJS = $(TMP_DLL_OBJS:obj\vc=obj\dll\vc)

#**********************************************************
# C compiler, Harbour compiler and Linker flags.
#**********************************************************

# Main "Include" directory
INCLUDE_DIR    = include

#**********************************************************

# In which mode compile Harbour C or CPP
!if "$(HB_BUILD_MODE)" == "cpp"
HB_BUILD_MODE  = P
!else
HB_BUILD_MODE  = C
!endif

#**********************************************************

# C Compiler Flags
!if $(HB_VISUALC_VER) >= 80
CFLAGS_VER     = -Od -Os -Gy -GS- -EHsc- -fp:fast -Gm -Zi -GR- -FD -D_CRT_SECURE_NO_DEPRECATE
!else
CFLAGS_VER     = -Oxsb1 -EHsc /YX /GF
!endif

CFLAGS         = -I$(INCLUDE_DIR) -I$(CFLAGS_VER) -T$(HB_BUILD_MODE) -W3 -nologo \
                 -D"_WIN32_WCE=0x420" -D"UNDER_CE=0x420" -D"WIN32_PLATFORM_PSPC" \
                 -D"WINCE" -D"_WINCE" -D"_WINDOWS" -D"ARM" -D"_ARM_" -D"ARMV4" \
                 -D"POCKETPC2003_UI_MODEL" -D"_M_ARM" -D"UNICODE" -D"_UNICODE" \
                 -DHB_WINCE $(C_USR) $(CFLAGS) -D_UWIN -I$(OBJ_DIR)
#     -D"_CONSOLE"

#-----------
!ifndef HB_WINCE_COMPILE_WITH_GTWIN
CFLAGS    = $(CFLAGS) -DHB_NO_WIN_CONSOLE
!endif
#-----------
!if "$(HB_BUILD_DEBUG)" == "yes"
CFLAGS    = $(CFLAGS) -D "_DEBUG" -D "DEBUG"
DBGMARKER = d
!else
CFLAGS    = $(CFLAGS) -D "NDEBUG"
!endif
#-----------
!if "$(HB_BUILD_ST)" != "yes"
CFLAGS    = -MT$(DBGMARKER) $(CFLAGS)
!endif
#-----------
!if "$(HB_GT_DEFAULT)" != ""
CFLAGS    = -D"HB_GT_DEFAULT=$(HB_GT_DEFAULT:gt=)" $(CFLAGS)
!endif
#-----------
!if "$(HB_GT_LIB)" != ""
CFLAGS    = -D"HB_GT_LIB=$(HB_GT_LIB:gt=)" $(CFLAGS)
!endif
#-----------

#**********************************************************

CLIBFLAGS      = -c $(CFLAGS) $(CLIBFLAGS)
CLIBFLAGSDLL   = -D__EXPORT__ $(CLIBFLAGS) $(CLIBFLAGSDLL)
CEXEFLAGSDLL   =  $(CLIBFLAGS) $(CEXEFLAGSDLL)

#**********************************************************

# Harbour Compiler Flags
HBFLAGSCMN     = -DHB_WINCE -D__PLATFORM__WINCE -i$(INCLUDE_DIR) -q0 -w3 -es2 -gc1 $(PRG_USR)
HARBOURFLAGS   = -n $(HBFLAGSCMN) $(HARBOURFLAGS)
HARBOURFLAGSDLL= -D__EXPORT__ -n1 $(HBFLAGSCMN) $(HARBOURFLAGSDLL)

#**********************************************************

# Linker Flags
LDFLAGS        = /NOLOGO /SUBSYSTEM:windowsce,4.20 /MACHINE:ARM /ARMPADCODE \
                 /STACK:65536,4096 /ALIGN:4096 /NODEFAULTLIB:"oldnames.lib" \
                 /NODEFAULTLIB:"kernel32.lib" /MANIFEST:NO /LIBPATH:$(LIB_DIR) \
                 /OPT:REF /OPT:ICF $(LDFLAGS)
#                /ERRORREPORT:PROMPT /ENTRY:"mainWCRTStartup"
!if $(HB_VISUALC_VER) >= 80
LDFLAGS        = $(LDFLAGS) /MANIFEST:NO
!endif

LDFLAGSDLL     = /NOLOGO /DLL /MACHINE:ARM /ARMPADCODE \
                 /STACK:65536,4096 /ALIGN:4096 /NODEFAULTLIB:"oldnames.lib" \
                 /LIBPATH:$(LIB_DIR) $(LDFLAGSDLL)

!if "$(HB_BUILD_DEBUG)" == "yes"
LDFLAGS        = /DEBUG $(LDFLAGS)
LDFLAGSDLL     = /DEBUG $(LDFLAGSDLL)
!endif

#**********************************************************
# COMPILE Rules
#**********************************************************

#*******************************************************
# General *.c --> *.obj COMPILE rules for STATIC Libraries
#*******************************************************
{$(OBJ_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(MAIN_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(COMMON_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(COMPILER_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(PP_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(VM_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(RTL_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(MACRO_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(DEBUG_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(LANG_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(CODEPAGE_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(PCRE_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(HBZLIB_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(RDD_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(NULSYS_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(DBFNTX_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(DBFCDX_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(DBFFPT_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(HBSIX_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(HSX_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(USRRDD_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(GTCGI_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(GTPCA_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(GTSTD_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(GTWIN_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(GTWVT_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(GTGUI_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(COMPILER_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(HBPPTEST_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(HBRUN_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(HBDOT_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(HBTEST_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(HBDOC_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(HBMAKE_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************
{$(HBVER_DIR)}.c{$(OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $<
#*******************************************************

#*******************************************************
# General *.prg --> *.obj COMPILE rules for STATIC Libraries
#*******************************************************
{$(OBJ_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(COMMON_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(PP_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(VM_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(RTL_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(MACRO_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(DEBUG_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(LANG_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(CODEPAGE_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(PCRE_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(HBZLIB_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(RDD_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(NULSYS_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(DBFNTX_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(DBFCDX_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(DBFFPT_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(HBSIX_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(HSX_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(USRRDD_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(GTCGI_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(GTPCA_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(GTSTD_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(GTWIN_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(GTWVT_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(GTGUI_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(COMPILER_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(HBPPTEST_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) $< -o$(OBJ_DIR)\\
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(HBRUN_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(HBDOT_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(HBTEST_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(HBDOC_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(HBMAKE_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************
{$(HBVER_DIR)}.prg{$(OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGS) -Fo$(OBJ_DIR)\ $(OBJ_DIR)\$(*B).c
#*******************************************************

#*******************************************************
# General *.c --> *.obj COMPILE rules for SHARED Libraries
#*******************************************************
{$(DLL_OBJ_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(MAIN_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(COMMON_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(COMPILER_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(PP_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(VM_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(RTL_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(MACRO_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(DEBUG_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(LANG_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(CODEPAGE_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(PCRE_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(HBZLIB_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(RDD_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(NULSYS_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(DBFNTX_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(DBFCDX_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(DBFFPT_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(HBSIX_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(HSX_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(USRRDD_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(GTCGI_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(GTPCA_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(GTSTD_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(GTWIN_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(GTWVT_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(GTGUI_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************

#*******************************************************
# General *.c --> *.obj COMPILE rules for EXECUTABLES,
# which use Harbour SHARED Library compiled as DLL
#*******************************************************
#{$(COMPILER_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
#    $(CC) $(CEXEFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(HBPPTEST_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CEXEFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(HBRUN_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CEXEFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(HBDOT_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CEXEFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(HBTEST_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CEXEFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(HBDOC_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CEXEFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(HBMAKE_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CEXEFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************
{$(HBVER_DIR)}.c{$(DLL_OBJ_DIR)}$(OBJEXT)::
    $(CC) $(CEXEFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $<
#*******************************************************

#*******************************************************
# General *.prg --> *.obj COMPILE rules for SHARED Libraries
#*******************************************************
{$(DLL_OBJ_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(COMMON_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(PP_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(VM_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(RTL_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(MACRO_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(DEBUG_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(LANG_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(PCRE_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(HBZLIB_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(CODEPAGE_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(RDD_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(NULSYS_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(DBFNTX_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(DBFCDX_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(DBFFPT_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(HBSIX_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(HSX_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(USRRDD_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(GTCGI_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(GTPCA_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(GTSTD_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(GTWIN_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(GTWVT_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(GTGUI_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGSDLL) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CLIBFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************

#*******************************************************
# General *.prg --> *.obj COMPILE rules for EXECUTABLES,
# which use Harbour SHARED Library compiled as DLL
#*******************************************************
#{$(COMPILER_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
#    $(HB) $(HARBOURFLAGS) -o$(DLL_OBJ_DIR)\ $<
#    $(CC) $(CEXEFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(HBPPTEST_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CEXEFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(HBRUN_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CEXEFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(HBDOT_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CEXEFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(HBTEST_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CEXEFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(HBDOC_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CEXEFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(HBMAKE_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CEXEFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#*******************************************************
{$(HBVER_DIR)}.prg{$(DLL_OBJ_DIR)}$(OBJEXT):
    $(HB) $(HARBOURFLAGS) -o$(DLL_OBJ_DIR)\ $<
    $(CC) $(CEXEFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $(DLL_OBJ_DIR)\$(*B).c
#**********************************************************

#**********************************************************
# TARGET dependencies
#**********************************************************

all : $(HB_DEST_DIRS) $(HB_BUILD_TARGETS)

#**********************************************************
# Helper targets - disabled for Msvc
#**********************************************************

#BasicLibs : $(COMMON_LIB) $(COMPILER_LIB) $(PP_LIB)
#BasicExes : $(HARBOUR_EXE)
#StdLibs   : $(STANDARD_STATIC_HBLIBS)

#**********************************************************

$(HB_DEST_DIRS) $(HB_BIN_INSTALL) $(HB_LIB_INSTALL) $(HB_INC_INSTALL):
    !if not exist $@\nul mkdir $@

#**********************************************************
# LIBRARY Targets BUILD rules
#**********************************************************
$(COMMON_LIB)   : $(COMMON_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(PP_LIB)       : $(PP_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(COMPILER_LIB) : $(COMPILER_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(VM_LIB)       : $(VM_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(RTL_LIB)      : $(RTL_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(MACRO_LIB)    : $(MACRO_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(DEBUG_LIB)    : $(DEBUG_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(LANG_LIB)     : $(LANG_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(CODEPAGE_LIB) : $(CODEPAGE_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(PCRE_LIB)     : $(PCRE_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(HBZLIB_LIB)   : $(HBZLIB_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(RDD_LIB)      : $(RDD_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(NULSYS_LIB)   : $(NULSYS_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(DBFNTX_LIB)   : $(DBFNTX_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(DBFCDX_LIB)   : $(DBFCDX_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(DBFFPT_LIB)   : $(DBFFPT_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(HBSIX_LIB)    : $(HBSIX_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(HSX_LIB)      : $(HSX_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(USRRDD_LIB)   : $(USRRDD_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(GTCGI_LIB)    : $(GTCGI_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(GTDOS_LIB)    : $(GTDOS_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(GTPCA_LIB)    : $(GTPCA_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(GTSTD_LIB)    : $(GTSTD_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(GTWIN_LIB)    : $(GTWIN_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(GTWVT_LIB)    : $(GTWVT_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************
$(GTGUI_LIB)    : $(GTGUI_LIB_OBJS)
    $(MKLIB) /out:$@ $**
#**********************************************************

#**********************************************************
# EXECUTABLE Targets
#**********************************************************

#**********************************************************
# HARBOUR build rule
#**********************************************************
$(HARBOUR_EXE) : $(HARBOUR_EXE_OBJS)
    IF EXIST "$(HARBOUR_EXE)" $(DEL) "$(HARBOUR_EXE)" > nul
    $(LINKER) @<<
$(LDFLAGS)
/OUT:$(HARBOUR_EXE)
$(**: = ^
)
$(COMMON_LIB)
$(COMPILER_LIB)
$(PP_LIB)
coredll.lib corelibc.lib
<<$(HB_KEEPSTATE)
#**********************************************************
# HBPP build rule
#**********************************************************
$(HBPP_EXE) : $(HBPP_EXE_OBJS)
    IF EXIST "$(HBPP_EXE)" $(DEL) "$(HBPP_EXE)" > nul
    $(LINKER) @<<
$(LDFLAGS)
/OUT:$(HBPP_EXE)
$(**: = ^
)
$(COMMON_LIB)
coredll.lib corelibc.lib
<<$(HB_KEEPSTATE)
#**********************************************************
# HBRUN build rule
#**********************************************************
$(HBRUN_EXE)  : $(HBRUN_EXE_OBJS)
    IF EXIST "$(HBRUN_EXE)" $(DEL) "$(HBRUN_EXE)" > nul
    $(LINKER) @<<
$(LDFLAGS)
/OUT:$(HBRUN_EXE)
$(**: = ^
)
$(STANDARD_STATIC_HBLIBS)
coredll.lib corelibc.lib winsock.lib ws2.lib
<<$(HB_KEEPSTATE)
#**********************************************************
# HBDOT build rule
#**********************************************************
$(HBDOT_EXE)  : $(HBDOT_EXE_OBJS)
    IF EXIST "$(HBDOT_EXE)" $(DEL) "$(HBDOT_EXE)" > nul
    $(LINKER) @<<
$(LDFLAGS)
/OUT:$(HBDOT_EXE)
$(**: = ^
)
$(STANDARD_STATIC_HBLIBS)
coredll.lib corelibc.lib winsock.lib ws2.lib
<<$(HB_KEEPSTATE)
#**********************************************************
# HBTEST build rule
#**********************************************************
$(HBTEST_EXE) : $(HBTEST_EXE_OBJS)
    IF EXIST "$(HBTEST_EXE)" $(DEL) "$(HBTEST_EXE)" > nul
    $(LINKER) @<<
$(LDFLAGS)
/OUT:$(HBTEST_EXE)
$(**: = ^
)
$(STANDARD_STATIC_HBLIBS)
coredll.lib corelibc.lib winsock.lib ws2.lib
<<$(HB_KEEPSTATE)
#**********************************************************
# HBPPTEST build rule
#**********************************************************
$(HBPPTEST_EXE) : $(HBPPTEST_EXE_OBJS)
    IF EXIST "$(HBPPTEST_EXE)" $(DEL) "$(HBPPTEST_EXE)" > nul
    $(LINKER) @<<
$(LDFLAGS)
/OUT:$(HBPPTEST_EXE)
$(**: = ^
)
$(PP_LIB)
$(STANDARD_STATIC_HBLIBS)
coredll.lib corelibc.lib winsock.lib ws2.lib
<<$(HB_KEEPSTATE)
#**********************************************************
# HBDOC build rule
#**********************************************************
$(HBDOC_EXE)  : $(HBDOC_EXE_OBJS)
    IF EXIST "$(HBDOC_EXE)" $(DEL) "$(HBDOC_EXE)" > nul
    $(LINKER) @<<
$(LDFLAGS)
/OUT:$(HBDOC_EXE)
$(**: = ^
)
$(STANDARD_STATIC_HBLIBS)
$(HBDOC_LIBS)
coredll.lib corelibc.lib winsock.lib ws2.lib
<<$(HB_KEEPSTATE)
#**********************************************************
# HBMAKE build rule
#**********************************************************
$(HBMAKE_EXE) : $(HBMAKE_EXE_OBJS)
    IF EXIST "$(HBMAKE_EXE)" $(DEL) "$(HBMAKE_EXE)" > nul
    $(LINKER) @<<
$(LDFLAGS)
/OUT:$(HBMAKE_EXE)
$(**: = ^
)
$(STANDARD_STATIC_HBLIBS)
coredll.lib corelibc.lib winsock.lib ws2.lib
<<$(HB_KEEPSTATE)
#**********************************************************
# HBVER build rule
#**********************************************************
$(HBVER_EXE)  : $(HBVER_EXE_OBJS)
    IF EXIST "$(HBVER_EXE)" $(DEL) "$(HBVER_EXE)" > nul
    $(LINKER) @<<
$(LDFLAGS)
/OUT:$(HBVER_EXE)
$(**: = ^
)
$(STANDARD_STATIC_HBLIBS)
coredll.lib corelibc.lib winsock.lib ws2.lib
<<$(HB_KEEPSTATE)
#**********************************************************

#**********************************************************
# DLL Targets
#**********************************************************
$(HARBOUR_DLL) : $(HB) $(DLL_OBJS)
    $(LINKER) @<<
$(LDFLAGSDLL) /OUT:$(@)
/IMPLIB:$(@:.dll=.lib)
$(DLL_OBJS: = ^
)
coredll.lib gdi32.lib
<<$(HB_KEEPSTATE)
#**********************************************************
# DLL EXECUTABLE Targets
#**********************************************************
HBTESTDLL_OBJS =  $(DLL_OBJ_DIR)\mainstd$(OBJEXT) $(HBTEST_EXE_OBJS:obj\vc=obj\dll\vc)
$(HBTESTDLL_EXE) : $(HARBOUR_DLL) $(HBTESTDLL_OBJS)
    $(LINKER) @<<
$(LDFLAGS)
/OUT:$(HBTESTDLL_EXE)
$(HBTESTDLL_OBJS: = ^
)
$(HARBOUR_DLL:.dll=.lib)
<<$(HB_KEEPSTATE)
#----------------------------------------------------------
$(DLL_OBJ_DIR)\mainstd$(OBJEXT) : $(VM_DIR)\mainstd.c
    $(CC) $(CEXEFLAGSDLL) -Fo$(DLL_OBJ_DIR)\ $**
#**********************************************************

#**********************************************************
# EXTRA Object's DEPENDENCIES
#**********************************************************

# Generated by an intermediate utility hbpp.exe
# built at the initial phase of build process
$(OBJ_DIR)\pptable.obj     : $(OBJ_DIR)\pptable.c
$(DLL_OBJ_DIR)\pptable.obj : $(DLL_OBJ_DIR)\pptable.c

$(OBJ_DIR)\pptable.c     : include\hbstdgen.ch include\std.ch ChangeLog $(PP_DIR)\ppcore.c $(PP_DIR)\hbpp.c
    IF EXIST "$(OBJ_DIR)\pptable.c" $(DEL) "$(OBJ_DIR)\pptable.c" > nul
    $(HBPP) include/hbstdgen.ch -o$(OBJ_DIR)/pptable.c -q -cChangeLog -vinclude/hbverbld.h

$(DLL_OBJ_DIR)\pptable.c : include\hbstdgen.ch include\std.ch ChangeLog $(PP_DIR)\ppcore.c $(PP_DIR)\hbpp.c
    IF EXIST "$(DLL_OBJ_DIR)\pptable.c" $(DEL) "$(DLL_OBJ_DIR)\pptable.c" > nul
    $(HBPP) include/hbstdgen.ch -o$(DLL_OBJ_DIR)/pptable.c -q -cChangeLog -vinclude/hbverbld.h

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
# CLEAN rules
#**********************************************************

clean: doClean
Clean: doClean
CLEAN: doClean

doClean:
    -if exist *.idb            $(DEL) *.idb > nul
    -if exist *.pch            $(DEL) *.pch > nul
    -if exist *.pdb            $(DEL) *.pdb > nul
    -if exist $(OBJ_DIR)\*.obj $(DEL) $(OBJ_DIR)\*.obj > nul
    -if exist $(OBJ_DIR)\*.c   $(DEL) $(OBJ_DIR)\*.c   > nul
    -if exist $(OBJ_DIR)\*.h   $(DEL) $(OBJ_DIR)\*.h   > nul
    -if exist $(OBJ_DIR)\*.pch $(DEL) $(OBJ_DIR)\*.pch > nul
    -if exist $(LIB_DIR)\*.lib $(DEL) $(LIB_DIR)\*.lib > nul
    -if exist $(BIN_DIR)\*.exe $(DEL) $(BIN_DIR)\*.exe > nul
    -if exist $(BIN_DIR)\*.pdb $(DEL) $(BIN_DIR)\*.pdb > nul
    -if exist $(BIN_DIR)\*.ilk $(DEL) $(BIN_DIR)\*.ilk > nul
    -if exist $(BIN_DIR)\*.map $(DEL) $(BIN_DIR)\*.map > nul
    -if exist $(BIN_DIR)\*.dll $(DEL) $(BIN_DIR)\*.dll > nul
    -if exist $(BIN_DIR)\*.lib $(DEL) $(BIN_DIR)\*.lib > nul
    -if exist $(BIN_DIR)\*.exp $(DEL) $(BIN_DIR)\*.exp > nul
    -if exist $(DLL_OBJ_DIR)\*.obj   $(DEL) $(DLL_OBJ_DIR)\*.obj   > nul
    -if exist $(DLL_OBJ_DIR)\*.c     $(DEL) $(DLL_OBJ_DIR)\*.c     > nul
    -if exist $(DLL_OBJ_DIR)\*.h     $(DEL) $(DLL_OBJ_DIR)\*.h     > nul
    -if exist inst_$(HB_CC_NAME).log $(DEL) inst_$(HB_CC_NAME).log > nul
    -if exist bin\*.exe        $(DEL) bin\*.exe > nul
    -if exist lib\*.lib        $(DEL) lib\*.lib > nul

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
