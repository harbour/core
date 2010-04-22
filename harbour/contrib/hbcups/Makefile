#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbcups

C_SOURCES := \
   hbcups.c \

PRG_HEADERS := \
   hbcairo.ch \

_DET_DSP_NAME := cups
_DET_VAR_INC_ := HB_INC_CUPS
_DET_VAR_HAS_ := HB_HAS_CUPS
_DET_FLT_PLAT :=
_DET_FLT_COMP :=
_DET_INC_DEFP := /usr/include /opt/local/include
_DET_INC_HEAD := /cups/cairo.h
include $(TOP)$(ROOT)config/detfun.mk

ifneq ($(HB_HAS_CUPS),)
   HB_CFLAGS += $(foreach d,$(HB_HAS_CUPS),-I$(d))

   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := $(_DET_RES_TEXT)
   include $(TOP)$(ROOT)config/none.mk
endif
