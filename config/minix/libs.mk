SYSLIBPATHS := /usr/lib /usr/pkg/lib

ifneq ($(HB_LINKING_RTL),)
   ifeq ($(HB_LIBNAME_CURSES),)
      HB_LIBNAME_CURSES := curses
   endif
   ifneq ($(HB_HAS_CURSES),)
      SYSLIBS += $(HB_LIBNAME_CURSES)
   endif
   ifneq ($(HB_HAS_SLANG),)
      SYSLIBS += slang
   endif
   ifneq ($(HB_HAS_X11),)
      SYSLIBS += X11
      SYSLIBPATHS += /usr/pkg/X11R6/lib
   endif
   ifneq ($(HB_HAS_PCRE),)
      ifeq ($(HB_HAS_PCRE_LOCAL),)
         SYSLIBS += pcre
      endif
   endif
   ifeq ($(HB_HAS_ZLIB_LOCAL),)
      SYSLIBS += z
   endif
endif

SYSLIBS += m
