#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.cf

LIBNAME := sddfb

ifneq ($(HB_COMPILER),pocc64)

ifeq ($(HB_INC_FIREBIRD),)
ifeq ($(HB_XBUILD),)
HB_INC_FIREBIRD := /usr/include /opt/firebird/include
endif
endif

HB_INC_FIREBIRD_OK := $(foreach d, $(HB_INC_FIREBIRD), $(if $(wildcard $(d)/ibase.h),$(d),))

ifneq ($(strip $(HB_INC_FIREBIRD_OK)),)

HB_USER_CFLAGS += $(foreach d, $(HB_INC_FIREBIRD_OK), -I$(d))

C_SOURCES := \
   fbirddd.c \

include $(TOP)$(ROOT)config/lib.cf
else
include $(TOP)$(ROOT)config/none.cf
endif
else
include $(TOP)$(ROOT)config/none.cf
endif
