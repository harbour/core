#
# $Id$
#

ROOT = ../../../

LIBNAME=sddfb

ifeq ($(HB_INC_FBSQL),)
ifeq ($(HB_XBUILD),)
HB_INC_FIREBIRD = /usr/include /opt/firebird/include
endif
endif

HB_INC_FIREBIRD_OK += $(foreach d, $(HB_INC_FIREBIRD), $(if $(wildcard $(d)/ibase.h),$(d),))

ifneq ($(strip $(HB_INC_FIREBIRD_OK)),)

C_USR += $(foreach d, $(HB_INC_FIREBIRD_OK), -I$(d))

C_SOURCES=\
	fbsqldd.c \

include $(TOP)$(ROOT)config/lib.cf
else
include $(TOP)$(ROOT)config/none.cf
endif
