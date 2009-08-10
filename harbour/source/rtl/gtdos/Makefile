#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.cf

LIBNAME := gtdos

ifeq ($(HB_ARCHITECTURE),dos)

C_SOURCES := \
   gtdos.c \

include $(TOP)$(ROOT)config/lib.cf

else
include $(TOP)$(ROOT)config/none.cf
endif
