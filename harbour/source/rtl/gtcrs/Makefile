#
# $Id$
#

ROOT = ../../../

LIBNAME=gtcrs

C_SOURCES=\
	gtcrs.c \

ifeq ($(HB_INC_CURSES),)
ifeq ($(HB_XBUILD),)
ifeq ($(HB_NCURSES_194),yes)
HB_INC_CURSES = /usr/include/ncur194
else
HB_INC_CURSES = /usr/include /usr/include/ncurses /usr/local/include /usr/local/include/ncurses /sw/include /sw/include/ncurses /opt/local/include /opt/local/include/ncurses
endif
endif
endif

ifeq ($(HB_WITHOUT_GTCRS),yes)
HB_INC_CURSES =
endif

HB_INC_CURSES_OK += $(foreach d, $(HB_INC_CURSES), $(if $(wildcard $(d)/curses.h),$(d),))
HB_INC_CURSES_OK:=$(strip $(HB_INC_CURSES_OK))

ifeq ($(HB_INC_CURSES_OK),)
ifeq ($(HB_ARCHITECTURE),dos)
HB_INC_CURSES_OK += $(foreach d, $(subst ;, ,$(PATH)), $(if $(wildcard $(d)/../include/curses.h),$(d)/../include,))
HB_INC_CURSES_OK:=$(strip $(HB_INC_CURSES_OK))
endif
endif

ifneq ($(HB_INC_CURSES_OK),)

HB_INC_GPM_OK =
ifneq ($(HB_GPM_MOUSE),no)
ifneq ($(HB_COMMERCE),yes)
HB_INC_GPM = /usr/include /usr/local/include
HB_INC_GPM_OK += $(foreach d, $(HB_INC_GPM), $(if $(wildcard $(d)/gpm.h),$(d),))
endif
endif

ifeq ($(HB_GT_CRS_BCEHACK),yes)
  HB_USER_CFLAGS += -DHB_GT_CRS_BCEHACK
endif
ifneq ($(strip $(HB_INC_GPM_OK)),)
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
