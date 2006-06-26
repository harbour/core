#
# $Id$
#

ROOT = ../../../

C_SOURCES=\
	gtgui.c \
	gtdef.c \

LIBNAME=gtgui

include $(TOP)$(ROOT)config/lib.cf

ifneq ($(HB_GT_DEFAULT),)
CFLAGS += -DHB_GT_DEFAULT=$(HB_GT_DEFAULT:gt%=%)
endif
ifneq ($(HB_GT_LIB),)
CFLAGS += -DHB_GT_LIB=$(HB_GT_LIB:gt%=%)
endif
