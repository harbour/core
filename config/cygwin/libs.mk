SYSLIBPATHS :=

ifneq ($(HB_LINKING_RTL),)
   ifeq ($(HB_LIBNAME_CURSES),)
      HB_LIBNAME_CURSES := ncurses
   endif
   ifneq ($(HB_HAS_CURSES),)
      SYSLIBS += $(HB_LIBNAME_CURSES)
   endif
   ifneq ($(HB_HAS_SLANG),)
      SYSLIBS += slang
   endif
   ifneq ($(HB_HAS_X11),)
      SYSLIBS += X11
   endif
   ifneq ($(HB_HAS_PCRE2),)
      ifeq ($(HB_HAS_PCRE2_LOCAL),)
         SYSLIBS += pcre2
      endif
   else
   ifneq ($(HB_HAS_PCRE),)
      ifeq ($(HB_HAS_PCRE_LOCAL),)
         SYSLIBS += pcre
      endif
   endif
   endif
   ifeq ($(HB_HAS_ZLIB_LOCAL),)
      SYSLIBS += z
   endif
   SYSLIBS += rt dl
   # Don't seem to be needed here, but added it for reference to move/copy it to *nix platforms where this is required
   ifneq ($(HB_LINKING_VMMT),)
      SYSLIBS += pthread
   endif
endif

SYSLIBS += m
