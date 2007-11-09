#
# $Id$
#

# Makefile common section for Harbour Project Contrib libs
# for Microsoft Visual C/Borland C/C++
# -----------------------------------------------------------------

#**********************************************************

# NOTE: "echo." intentionally used instead of "echo", to avoid conflicts
#   	with external commands named echo.

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

.SUFFIXES: $(LIBEXT) $(OBJEXT) .prg .c .asm

#**********************************************************
# Directory macros. These should never have to change.
#**********************************************************

OBJ_DIR = ..\..\obj\$(CC_NAME)
LIB_DIR = ..\..\lib\$(CC_NAME)
BIN_DIR = ..\..\bin\$(CC_NAME)

INCLUDE_DIR = ..\..\include

#**********************************************************
# Macros to access Harbour executable and other goodies
#**********************************************************

HARBOUR_EXE = $(BIN_DIR)\harbour.exe

#**********************************************************
