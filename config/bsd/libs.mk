
SYSLIBPATHS :=

ifneq ($(HB_LINKING_RTL),)
   ifeq ($(HB_LIBNAME_CURSES),)
      HB_LIBNAME_CURSES := curses
   endif
   ifneq ($(HB_HAS_CURSES),)
      SYSLIBS += $(HB_LIBNAME_CURSES)
   endif
   ifneq ($(HB_HAS_SLANG),)
      ifneq ($(wildcard /usr/pkg/lib/libslang2.so),)
         SYSLIBS += slang2
      else
         SYSLIBS += slang
      endif
      # In BSD, slang still needs curses :(
      ifneq ($(HB_HAS_CURSES),)
         SYSLIBS += $(HB_LIBNAME_CURSES)
      endif
   endif
   ifneq ($(HB_HAS_X11),)
      SYSLIBS += X11
      SYSLIBPATHS += /usr/X11R6/lib
   endif

   SYSLIBPATHS += /usr/local/lib /usr/pkg/lib

   ifneq ($(HB_HAS_PCRE),)
      ifeq ($(HB_HAS_PCRE_LOCAL),)
         SYSLIBS += pcre
      endif
   endif
   ifeq ($(HB_HAS_ZLIB_LOCAL),)
      SYSLIBS += z
   endif

   ifneq ($(HB_LINKING_VMMT),)
      SYSLIBS += pthread
   endif
endif

SYSLIBS += m
