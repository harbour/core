#
# $Id$
#

SYSLIBS :=
SYSLIBPATHS :=

ifneq ($(HB_LINKING_RTL),)
   ifeq ($(HB_CRS_LIB),)
      HB_CRS_LIB := ncurses
   endif
   ifneq ($(HB_HAS_CURSES),)
      SYSLIBS += $(HB_CRS_LIB)
   endif
   ifneq ($(HB_HAS_SLANG),)
      SYSLIBS += slang
   endif
   ifneq ($(HB_HAS_X11),)
      SYSLIBS += X11
      SYSLIBPATHS += /usr/X11R6/lib
   endif
   ifneq ($(HB_HAS_PCRE),)
      ifeq ($(HB_HAS_PCRE_LOCAL),)
         SYSLIBS += pcre
      endif
   endif
   ifeq ($(HB_HAS_ZLIB_LOCAL),)
      SYSLIBS += z
   endif
   SYSLIBS += rt
endif

SYSLIBS += m
