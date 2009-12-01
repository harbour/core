#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbcairo

C_SOURCES := \
   context.c \
   core.c \
   image.c \
   paths.c \
   pdf.c \
   png.c \
   text.c \

PRG_HEADERS := \
   hbcairo.ch \

_DET_DSP_NAME := cairo
_DET_VAR_INC_ := HB_INC_CAIRO
_DET_VAR_HAS_ := HB_HAS_CAIRO
_DET_FLT_PLAT :=
_DET_FLT_COMP :=
_DET_INC_DEFP := /usr/include
_DET_INC_HEAD := /cairo.h
include $(TOP)$(ROOT)config/detfun.mk

ifneq ($(HB_HAS_CAIRO),)
   HB_CFLAGS += $(foreach d,$(HB_HAS_CAIRO),-I$(d))

   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := $(_DET_RES_TEXT)
   include $(TOP)$(ROOT)config/none.mk
endif
