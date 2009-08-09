#
# $Id$
#

ROOT := ../../../

LIBNAME := gtgui

HB_WITH_GTGUI := no
ifeq ($(HB_ARCHITECTURE),win)
   HB_WITH_GTGUI := yes
endif
ifeq ($(HB_ARCHITECTURE),wce)
   HB_WITH_GTGUI := yes
endif

ifeq ($(HB_WITH_GTGUI),yes)

C_SOURCES := \
	gtgui.c \

include $(TOP)$(ROOT)config/lib.cf

else
include $(TOP)$(ROOT)config/none.cf
endif
