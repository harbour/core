#
# $Id$
#

all : first

BIN_EXT :=
DYN_EXT := .so
DYN_PREF := lib

HB_GT_LIBS += gttrm

ifeq ($(HB_SHELL),sh)
   ifneq ($(filter $(HB_COMPILER),gcc icc),)
      ifeq ($(filter -fPIC,$(HB_USER_CFLAGS)),)
         ifeq ($(filter -fpic,$(HB_USER_CFLAGS)),)
            _UNAME_M := $(shell uname -m)
            ifeq ($(findstring 86,$(_UNAME_M)),)
               HB_CFLAGS += -fPIC
            else
               ifneq ($(findstring 64,$(_UNAME_M)),)
                  HB_CFLAGS += -fPIC
               endif
            endif
         endif
      endif
   endif
endif

SYSLIBS :=
SYSLIBPATHS :=

ifneq ($(filter hbrtl, $(LIBS)),)
   ifeq ($(HB_CRS_LIB),)
      HB_CRS_LIB := ncurses
   endif
   ifneq ($(filter gtcrs, $(LIBS)),)
      SYSLIBS += $(HB_CRS_LIB)
   endif
   ifneq ($(filter gtsln, $(LIBS)),)
      SYSLIBS += slang
   endif
   ifneq ($(filter gtxwc, $(LIBS)),)
      SYSLIBS += X11
    # SYSLIBPATHS += /usr/X11R6/lib64
      SYSLIBPATHS += /usr/X11R6/lib
   endif
   ifeq ($(HB_GPM_MOUSE),yes)
      SYSLIBS += gpm
   endif
   ifneq ($(filter -DHB_PCRE_REGEX, $(HB_USER_CFLAGS)),)
      SYSLIBS += pcre
   endif
   ifneq ($(filter -DHB_EXT_ZLIB, $(HB_USER_CFLAGS)),)
      SYSLIBS += z
   endif
   SYSLIBS += rt dl
endif

SYSLIBS += m
