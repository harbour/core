#
# $Id$
#

SYSLIBS :=
SYSLIBPATHS :=

ifneq ($(filter hbrtl, $(LIBS)),)
   ifeq ($(HB_CRS_LIB),)
      HB_CRS_LIB := curses
   endif
   ifneq ($(filter gtcrs, $(LIBS)),)
      SYSLIBS += $(HB_CRS_LIB)
   endif
   ifneq ($(filter gtsln, $(LIBS)),)
      SYSLIBS += slang
   endif
   ifneq ($(filter gtxwc, $(LIBS)),)
      SYSLIBS += X11
    # SYSLIBPATHS += /usr/X11R6/lib64
      SYSLIBPATHS += /usr/X11R6/lib
   endif
   ifneq ($(filter -DHB_PCRE_REGEX, $(HB_USER_CFLAGS)),)
      SYSLIBS += pcre
   endif
   ifneq ($(filter -DHB_EXT_ZLIB, $(HB_USER_CFLAGS)),)
      SYSLIBS += z
   endif
   SYSLIBS += rt socket nsl resolv
endif

SYSLIBS += m
