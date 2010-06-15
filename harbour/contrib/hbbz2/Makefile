#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbbz2

C_SOURCES := \
   hbbz2.c \

PRG_HEADERS := \
   hbbz2.ch

_DET_DSP_NAME := bzip2
_DET_VAR_INC_ := HB_INC_BZIP2
_DET_VAR_HAS_ := HB_HAS_BZIP2
_DET_FLT_PLAT := !wce
_DET_FLT_COMP :=
_DET_INC_DEFP := /usr/include
_DET_INC_LOCL := external/bzip2
_DET_INC_HEAD := /bzlib.h
include $(TOP)$(ROOT)config/detfun.mk

ifneq ($(HB_HAS_BZIP2),)
   HB_CFLAGS += $(foreach d,$(HB_HAS_BZIP2),-I$(d))

   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := $(_DET_RES_TEXT)
   include $(TOP)$(ROOT)config/none.mk
endif
