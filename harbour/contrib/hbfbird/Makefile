#
# $Id$
#

ifeq ($(HB_ARCHITECTURE),linux)
ifeq ($(HB_INC_FIREBIRD),)
HB_INC_FIREBIRD = /opt/firebird/include
endif
endif

HB_INC_FIREBIRD_OK += $(foreach d, $(HB_INC_FIREBIRD), $(if $(wildcard $(d)/ibase.h),$(d),))

ifneq ($(HB_INC_FIREBIRD_OK),)

C_USR += $(foreach d, $(HB_INC_FIREBIRD_OK), -I$(d))

ROOT = ../../

C_SOURCES=\
	firebird.c \

PRG_SOURCES=\
	tfirebird.prg \

LIBNAME=hbfbird

include $(TOP)$(ROOT)config/lib.cf

endif
