#
# $Id$
#

ROOT = ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbhpdf

C_SOURCES := \
   harupdf.c \

PRG_HEADERS := \
   harupdf.ch \

_DET_DSP_NAME := libharu
_DET_VAR_INC_ := HB_INC_LIBHARU
_DET_VAR_HAS_ := HB_HAS_LIBHARU
_DET_FLT_PLAT :=
_DET_FLT_COMP :=
_DET_INC_DEFP :=
ifneq ($(HB_HOST_PLAT),dos)
   _DET_INC_DEFP += /usr/include /usr/local/include $(TOP)$(ROOT)external/libhpdf
endif
_DET_INC_HEAD := /hpdf.h
include $(TOP)$(ROOT)config/detfun.mk

ifneq ($(HB_HAS_LIBHARU),)

   HB_CFLAGS += $(foreach d,$(HB_HAS_LIBHARU),-I$(d))

   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk

else
   HB_SKIP_REASON := $(_DET_RES_TEXT)
   include $(TOP)$(ROOT)config/none.mk
endif
