#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := sddfb

C_SOURCES := \
   sddfb.c \

_DET_DSP_NAME := firebird
_DET_VAR_INC_ := HB_INC_FIREBIRD
_DET_VAR_HAS_ := HB_HAS_FIREBIRD
_DET_FLT_PLAT :=
_DET_FLT_COMP := !pocc64 !poccarm
_DET_INC_DEFP := /usr/include /opt/firebird/include /Library/Frameworks/Firebird.framework/Headers
_DET_INC_HEAD := /ibase.h
include $(TOP)$(ROOT)config/detfun.mk

ifneq ($(HB_HAS_FIREBIRD),)
   HB_CFLAGS += $(foreach d,$(HB_HAS_FIREBIRD),-I$(d))
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := $(_DET_RES_TEXT)
   include $(TOP)$(ROOT)config/none.mk
endif
