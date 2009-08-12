#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.cf

LIBNAME := gtcrs

C_SOURCES := \
   gtcrs.c \

ifeq ($(HB_INC_CURSES),)
   ifeq ($(HB_XBUILD),)
      ifeq ($(HB_NCURSES_194),yes)
         HB_INC_CURSES := /usr/include/ncur194
      else
         HB_INC_CURSES := /usr/include /usr/local/include /sw/include /opt/local/include
      endif
   endif
endif

ifeq ($(HB_WITHOUT_GTCRS),yes)
   HB_INC_CURSES :=
endif
ifeq ($(HB_ARCHITECTURE),os2)
   HB_INC_CURSES :=
endif

HB_INC_CURSES_OK += $(foreach d, $(HB_INC_CURSES), $(if $(wildcard $(d)/curses.h),$(d),))
HB_INC_CURSES_OK := $(strip $(HB_INC_CURSES_OK))

ifeq ($(HB_INC_CURSES_OK),)
   ifeq ($(HB_COMPILER),djgpp)
      HB_INC_CURSES_OK += $(foreach d, $(subst ;, ,$(PATH)), $(if $(wildcard $(d)/../include/curses.h),$(d)/../include,))
      HB_INC_CURSES_OK := $(strip $(HB_INC_CURSES_OK))
   endif
endif

ifneq ($(HB_INC_CURSES_OK),)

ifeq ($(HB_GT_CRS_BCEHACK),yes)
   HB_USER_CFLAGS += -DHB_GT_CRS_BCEHACK
endif
ifeq ($(HB_GPM_MOUSE),yes)
   HB_USER_CFLAGS += -DHAVE_GPM_H
   ifeq ($(HB_GPM_NOICE_DISABLE),yes)
      HB_USER_CFLAGS += -DHB_GPM_NOICE_DISABLE
   endif
endif
ifeq ($(HB_NCURSES_194),yes)
   HB_USER_CFLAGS += -DHB_NCURSES_194
endif

HB_USER_CFLAGS += $(foreach d, $(HB_INC_CURSES_OK), -I$(d))

include $(TOP)$(ROOT)config/lib.cf

else
include $(TOP)$(ROOT)config/none.cf
endif
