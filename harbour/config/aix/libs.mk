#
# $Id$
#

# When compiling and linking with -pthread, the library search path should 
# include -L/usr/lib/threads at the beginning of the path.
# http://www.ibm.com/developerworks/aix/library/au-gnu.html
# (libc is there)
ifeq ($(HB_LINKING_VMMT),yes)
   SYSLIBPATHS := /usr/lib/threads
else
   SYSLIBPATHS :=
endif

ifneq ($(HB_LINKING_RTL),)
   ifeq ($(HB_LIBNAME_CURSES),)
      HB_LIBNAME_CURSES := xcurses
   endif
   ifneq ($(HB_HAS_CURSES),)
      SYSLIBS += $(HB_LIBNAME_CURSES)
   endif
   ifneq ($(HB_HAS_SLANG),)
      SYSLIBS += slang
   endif
   ifneq ($(HB_HAS_X11),)
      SYSLIBS += X11
      SYSLIBPATHS += /usr/X11R6/lib
   endif
   ifneq ($(HB_HAS_PCRE),)
      ifeq ($(HB_HAS_PCRE_LOCAL),)
         SYSLIBS += pcre
      endif
   endif
   ifeq ($(HB_HAS_ZLIB_LOCAL),)
      SYSLIBS += z
   endif
   SYSLIBS += rt

   ifneq ($(HB_LINKING_VMMT),)
      SYSLIBS += pthread
   endif
endif

SYSLIBS += m
