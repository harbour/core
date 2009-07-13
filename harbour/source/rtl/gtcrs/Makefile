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
HB_INC_CURSES = /usr/include/ncurses /sw/include /sw/include/ncurses
endif
endif
endif

ifeq ($(HB_WITHOUT_GTCRS),yes)
#HB_INC_CURSES =
endif

HB_INC_CURSES_OK += $(foreach d, $(HB_INC_CURSES), $(if $(wildcard $(d)/curses.h),$(d),))

ifneq ($(strip $(HB_INC_CURSES_OK)),)

HB_INC_GPM = /usr/include /usr/local/include
HB_INC_GPM_OK += $(foreach d, $(HB_INC_GPM), $(if $(wildcard $(d)/gpm.h),$(d),))

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
