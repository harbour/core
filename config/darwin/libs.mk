SYSLIBPATHS :=

ifneq ($(HB_LINKING_RTL),)
   ifeq ($(HB_LIBNAME_CURSES),)
      HB_LIBNAME_CURSES := ncurses
   endif
   ifneq ($(HB_HAS_CURSES),)
      SYSLIBS += $(HB_LIBNAME_CURSES)
      ifneq ($(wildcard /usr/local/opt/ncurses/lib),)
         SYSLIBPATHS += /usr/local/opt/ncurses/lib
      endif
   endif
   ifneq ($(HB_HAS_SLANG),)
      SYSLIBS += slang
      # In BSD, slang still needs curses :(
      ifneq ($(HB_HAS_CURSES),)
         SYSLIBS += $(HB_LIBNAME_CURSES)
      endif
      ifneq ($(wildcard /usr/local/lib),)
         SYSLIBPATHS += /usr/local/lib
      endif
      ifneq ($(wildcard /opt/local/lib),)
         SYSLIBPATHS += /opt/local/lib
      endif
      ifneq ($(wildcard /sw/lib),)
         SYSLIBPATHS += /sw/lib
      endif
   endif
   ifneq ($(HB_HAS_X11),)
      SYSLIBS += X11
      ifneq ($(wildcard /usr/X11R6/lib),)
         SYSLIBPATHS += /usr/X11R6/lib
      endif
      ifneq ($(wildcard /opt/X11/lib),)
         SYSLIBPATHS += /opt/X11/lib
      endif
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
endif

SYSLIBS += m
