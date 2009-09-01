#
# $Id$
#

# You can get the library from this link:
#    http://www.libgd.org/Downloads

# On Linux you'll need these packages:
#    gd, gd-devel

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbgd

C_SOURCES := \
   gdwrp.c \

PRG_HEADERS := \
   gd.ch \
   gdextern.ch \

PRG_SOURCES := \
   gd.prg \
   gdbar.prg \
   gdchart.prg \
   gdimage.prg \
   gdbarcod.prg \

_DET_DSP_NAME := libgd
_DET_VAR_INC_ := HB_INC_GD
_DET_VAR_HAS_ := HB_HAS_GD
_DET_FLT_PLAT :=
_DET_FLT_COMP :=
_DET_INC_DEFP := /usr/include /opt/local/include
_DET_INC_HEAD := /gd.h
include $(TOP)$(ROOT)config/detfun.mk

ifneq ($(HB_HAS_GD),)

   HB_CFLAGS += $(foreach d,$(HB_HAS_GD),-I$(d))

   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk

else
   HB_SKIP_REASON := $(_DET_RES_TEXT)
   include $(TOP)$(ROOT)config/none.mk
endif
