#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.cf

ifeq ($(HB_COMPILER),xcc)
   HB_USER_CFLAGS := $(subst -Ox,,$(subst -Ot,,$(HB_USER_CFLAGS)))
endif

C_SOURCES := \
	dbffpt1.c \

LIBNAME := rddfpt

include $(TOP)$(ROOT)config/lib.cf
