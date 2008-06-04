#
# $Id$
#

ROOT = ../../../

C_SOURCES=\
	gttrm.c \

LIBNAME=gttrm

include $(TOP)$(ROOT)config/lib.cf

ifeq ($(HB_GPM_MOUSE),yes)
  CFLAGS += -DHAVE_GPM_H
ifeq ($(HB_GPM_NOICE_DISABLE),yes)
  CFLAGS += -DHB_GPM_NOICE_DISABLE
endif
endif
