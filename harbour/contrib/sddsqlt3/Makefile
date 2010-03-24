#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := sddsqlt3

C_SOURCES := \
   sddsqlt3.c \

_DET_DSP_NAME := sqlite3
_DET_VAR_INC_ := HB_INC_SQLITE3
_DET_VAR_HAS_ := HB_HAS_SQLITE3
_DET_FLT_PLAT :=
_DET_FLT_COMP :=
_DET_INC_DEFP := /usr/include /boot/common/include
_DET_INC_LOCL := $(realpath $(TOP)$(ROOT)external/sqlite3)
_DET_INC_HEAD := /sqlite3.h
include $(TOP)$(ROOT)config/detfun.mk

ifneq ($(HB_HAS_SQLITE3),)
   HB_INC_DEPEND := -I$(TOP)$(ROOT)contrib/rddsql
   HB_CFLAGS += $(foreach d,$(HB_HAS_SQLITE3),-I$(d))
   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := $(_DET_RES_TEXT)
   include $(TOP)$(ROOT)config/none.mk
endif
