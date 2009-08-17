#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtdos

ifeq ($(HB_ARCHITECTURE),dos)

C_SOURCES := \
   gtdos.c \

include $(TOP)$(ROOT)config/lib.mk

else
include $(TOP)$(ROOT)config/none.mk
endif
