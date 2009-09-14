#
# $Id$
#

SYSLIBS :=
SYSLIBPATHS := /system/lib /boot/develop/abi/x86/gcc2/lib

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
      # add 64-bit lib dir needed for some distros (red hat)
      #ifneq ($(findstring 64,$(shell uname -m)),)
      #   SYSLIBPATHS += /usr/X11R6/lib64
      #endif
      #SYSLIBPATHS += /usr/X11R6/lib
   endif
   ifneq ($(HB_HAS_PCRE),)
      ifeq ($(HB_HAS_PCRE_LOCAL),)
         SYSLIBS += pcre
      endif
   endif
   ifeq ($(HB_HAS_ZLIB_LOCAL),)
      SYSLIBS += z
   endif
   SYSLIBS += root socket
endif

