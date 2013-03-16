
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
      # add 64-bit lib dir needed for some distros (Red Hat)
      ifneq ($(findstring 64,$(shell uname -m)),)
         SYSLIBPATHS += /usr/X11R6/lib64
      endif
      SYSLIBPATHS += /usr/X11R6/lib
   endif
   ifneq ($(HB_HAS_GPM),)
      SYSLIBS += gpm
   endif
   ifneq ($(HB_HAS_PCRE),)
      ifeq ($(HB_HAS_PCRE_LOCAL),)
         SYSLIBS += pcre
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
else
   ifeq ($(BUILD_SHARED),yes)
      SYSLIBS += pthread
   endif
endif

SYSLIBS += m
