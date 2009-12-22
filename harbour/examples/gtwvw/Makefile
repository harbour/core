#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtwvw

# NOTE: Has a huge number of warnings, which we don't want to see until fixed.
HB_BUILD_WARN := no
# NOTE: Due to large quantity of unprecise code, it doesn't build in C++ mode.
HB_BUILD_MODE := c

C_SOURCES := \
   gtwvw.c \
   wvwcheck.c \
   wvwdraw.c \
   wvwedit.c \
   wvwfuncs.c \
   wvwmenu.c \
   wvwpush.c \
   wvwstbar.c \
   wvwtbar.c \

ifeq ($(HB_PLATFORM),win)
   ifeq ($(filter $(HB_COMPILER),mingw64 msvc64 pocc64 iccia64 msvcia64),)
      include $(TOP)$(ROOT)config/header.mk
      include $(TOP)$(ROOT)config/lib.mk
   else
      HB_SKIP_REASON := compiler not supported
      include $(TOP)$(ROOT)config/none.mk
   endif
else
   HB_SKIP_REASON := platform not supported
   include $(TOP)$(ROOT)config/none.mk
endif
