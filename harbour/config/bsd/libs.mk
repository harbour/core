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
      # In BSD, slang still needs curses :(
      ifneq ($(HB_HAS_CURSES),)
         SYSLIBS += $(HB_CRS_LIB)
      endif
   endif
   ifneq ($(HB_HAS_X11),)
      SYSLIBS += X11
    # SYSLIBPATHS += /usr/X11R6/lib64
      SYSLIBPATHS += /usr/X11R6/lib
   endif

   SYSLIBPATHS += /usr/local/lib

   ifneq ($(filter -DHB_PCRE_REGEX, $(HB_USER_CFLAGS)),)
      SYSLIBS += pcre
   endif
   ifneq ($(filter -DHB_EXT_ZLIB, $(HB_USER_CFLAGS)),)
      SYSLIBS += z
   endif
endif

SYSLIBS += m
