#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.cf

LIBNAME := gtwvt

HB_WITH_GTWVT := no
ifeq ($(HB_ARCHITECTURE),win)
   HB_WITH_GTWVT := yes
endif
ifeq ($(HB_ARCHITECTURE),wce)
   HB_WITH_GTWVT := yes
endif

ifeq ($(HB_WITH_GTWVT),yes)

C_SOURCES := \
   gtwvt.c \

include $(TOP)$(ROOT)config/lib.cf

else
include $(TOP)$(ROOT)config/none.cf
endif
