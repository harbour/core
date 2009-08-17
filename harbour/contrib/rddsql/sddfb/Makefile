#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := sddfb

ifneq ($(HB_COMPILER),pocc64)

ifeq ($(HB_INC_FIREBIRD),)
   ifeq ($(HB_XBUILD),)
      HB_INC_FIREBIRD := /usr/include /opt/firebird/include
   endif
endif

HB_INC_FIREBIRD_OK := $(foreach d, $(HB_INC_FIREBIRD), $(if $(wildcard $(d)/ibase.h),$(d),))

ifneq ($(strip $(HB_INC_FIREBIRD_OK)),)

HB_CFLAGS += $(foreach d,$(HB_INC_FIREBIRD_OK),-I$(d))

C_SOURCES := \
   fbirddd.c \

include $(TOP)$(ROOT)config/lib.mk
else
include $(TOP)$(ROOT)config/none.mk
endif
else
include $(TOP)$(ROOT)config/none.mk
endif
