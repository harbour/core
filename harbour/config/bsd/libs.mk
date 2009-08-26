#
# $Id$
#

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
      # In BSD, slang still needs curses :(
      ifeq ($(filter gtcrs, $(LIBS)),)
         SYSLIBS += $(HB_CRS_LIB)
      endif
   endif
   ifneq ($(filter gtxwc, $(LIBS)),)
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
