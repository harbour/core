#
# $Id$
#

vpath %.c ../

ROOT = ../../../

C_SOURCES=\
	fm.c \

LIBNAME=hbfmmt

HB_USER_CFLAGS :=$(subst -DHB_FM_STATISTICS_OFF,,$(HB_USER_CFLAGS))
HB_USER_CFLAGS :=$(subst -DHB_FM_STATISTICS_DYN_OFF,,$(HB_USER_CFLAGS))
HB_USER_CFLAGS += -DHB_FM_STATISTICS -DHB_MT_VM

include $(TOP)$(ROOT)config/lib.cf
