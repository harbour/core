#
# $Id$
#

# Makefile common section for Harbour Project Contrib libs
# for Microsoft Visual C/Borland C/C++
# -----------------------------------------------------------------

#**********************************************************

# NOTE: "echo." intentionally used instead of "echo", to avoid conflicts
#       with external commands named echo.

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

OBJ_DIR = $(HB_ROOT)\obj\$(HB_CC_NAME)
LIB_DIR = $(HB_ROOT)\lib\$(HB_CC_NAME)
BIN_DIR = $(HB_ROOT)\bin\$(HB_CC_NAME)

INCLUDE_DIR = $(HB_ROOT)\include

#**********************************************************
# Macros to access Harbour executable and other goodies
#**********************************************************

HARBOUR_EXE = $(BIN_DIR)\harbour.exe

#**********************************************************
