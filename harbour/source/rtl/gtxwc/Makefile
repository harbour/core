#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.cf

LIBNAME := gtxwc

C_SOURCES := \
   gtxwc.c \

ifeq ($(HB_INC_X11),)
   ifeq ($(HB_XBUILD),)
      HB_INC_X11 := /usr/include
   endif
endif

ifeq ($(HB_WITHOUT_GTXWC),yes)
   HB_INC_X11 :=
endif

HB_INC_X11_OK := $(foreach d, $(HB_INC_X11), $(if $(wildcard $(d)/X11/Xlib.h),$(d),))

ifneq ($(strip $(HB_INC_X11_OK)),)

HB_USER_CFLAGS += $(foreach d, $(HB_INC_X11_OK), -I$(d))

include $(TOP)$(ROOT)config/lib.cf

else
include $(TOP)$(ROOT)config/none.cf
endif
