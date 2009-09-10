#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbmzip

C_SOURCES := \
   ioapi.c \
   zip.c \
   unzip.c \
   hbmzip.c

PRG_HEADERS := \
   hbmzip.ch

# force redetection
HB_HAS_ZLIB :=

_DET_DSP_NAME := zlib
_DET_VAR_INC_ := HB_INC_ZLIB
_DET_VAR_HAS_ := HB_HAS_ZLIB
_DET_FLT_PLAT :=
_DET_FLT_COMP :=
_DET_INC_DEFP := /usr/include
_DET_INC_LOCL :=
ifneq ($(filter $(HB_PLATFORM),win wce os2 dos),)
   _DET_INC_LOCL += $(TOP)$(ROOT)external/zlib
endif
_DET_INC_HEAD := /zlib.h

include $(TOP)$(ROOT)config/detfun.mk

ifneq ($(HB_HAS_ZLIB),)
   HB_CFLAGS += -DHB_HAS_ZLIB $(foreach d,$(HB_HAS_ZLIB),-I$(d))

   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := $(_DET_RES_TEXT)
   include $(TOP)$(ROOT)config/none.mk
endif
