#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtalleg

C_SOURCES := \
   gtalleg.c \
   ssf.c \

_DET_DSP_NAME := allegro
_DET_VAR_INC_ := HB_INC_ALLEGRO
_DET_VAR_HAS_ := HB_HAS_ALLEGRO
_DET_FLT_PLAT :=
_DET_FLT_COMP := !pocc !pocc64 !poccarm !xcc !dmc !watcom !mingwarm !msvcarm
_DET_INC_DEFP := /usr/include /opt/local/include
_DET_INC_HEAD := /allegro.h
include $(TOP)$(ROOT)config/detfun.mk

ifneq ($(HB_HAS_ALLEGRO),)

   HB_CFLAGS += $(foreach d,$(HB_HAS_ALLEGRO),-I$(d))

   include $(TOP)$(ROOT)config/lib.mk

   DIRS += gtallegs
   include $(TOP)$(ROOT)config/dir.mk
else
   HB_SKIP_REASON := $(_DET_RES_TEXT)
   include $(TOP)$(ROOT)config/none.mk
endif
