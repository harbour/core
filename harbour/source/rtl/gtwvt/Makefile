#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtwvt

HB_WITH_GTWVT := no
ifeq ($(HB_PLATFORM),win)
   HB_WITH_GTWVT := yes
endif
ifeq ($(HB_PLATFORM),wce)
   HB_WITH_GTWVT := yes
endif

ifeq ($(HB_WITH_GTWVT),yes)

C_SOURCES := \
   gtwvt.c \

include $(TOP)$(ROOT)config/lib.mk

else
include $(TOP)$(ROOT)config/none.mk
endif
