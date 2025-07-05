SYSLIBPATHS :=

ifneq ($(HB_LINKING_RTL),)
   ifneq ($(HB_HAS_X11),)
      SYSLIBS += X11
      ifneq ($(wildcard /usr/X11R6/lib),)
         SYSLIBPATHS += /usr/X11R6/lib
      endif
      ifneq ($(wildcard /opt/X11/lib),)
         SYSLIBPATHS += /opt/X11/lib
      endif
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
