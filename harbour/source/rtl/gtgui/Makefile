#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtgui

HB_WITH_GTGUI := no
ifeq ($(HB_PLATFORM),win)
   HB_WITH_GTGUI := yes
endif
ifeq ($(HB_PLATFORM),wce)
   HB_WITH_GTGUI := yes
endif

ifeq ($(HB_WITH_GTGUI),yes)

C_SOURCES := \
   gtgui.c \

include $(TOP)$(ROOT)config/lib.mk

else
include $(TOP)$(ROOT)config/none.mk
endif
