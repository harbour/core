#
# $Id$
#

ifeq ($(HB_COMPILER),xcc)
   HB_USER_CFLAGS := $(subst -Ox,,$(subst -Ot,,$(HB_USER_CFLAGS)))
endif

ROOT := ../../../

C_SOURCES := \
	dbffpt1.c \

LIBNAME := rddfpt

include $(TOP)$(ROOT)config/lib.cf
