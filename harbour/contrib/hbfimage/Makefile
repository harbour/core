#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbfimage

C_SOURCES := \
   fi_winfu.c \
   fi_wrp.c \

PRG_SOURCES := \

PRG_HEADERS := \
   freeimag.ch \

_DET_DSP_NAME := freeimage
_DET_VAR_INC_ := HB_INC_FREEIMAGE
_DET_VAR_HAS_ := HB_HAS_FREEIMAGE
# on dos and os2 it doesn't compile with watcom
_DET_FLT_PLAT := !dos !os2
_DET_FLT_COMP :=
_DET_INC_DEFP := /usr/include /opt/local/include
_DET_INC_HEAD := /FreeImage.h
include $(TOP)$(ROOT)config/detfun.mk

ifneq ($(HB_HAS_FREEIMAGE),)
   HB_CFLAGS += $(foreach d,$(HB_HAS_FREEIMAGE),-I$(d))

   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk

else
   HB_SKIP_REASON := $(_DET_RES_TEXT)
   include $(TOP)$(ROOT)config/none.mk
endif
