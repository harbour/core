#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbmzip

C_SOURCES := \
   hbmzip.c

PRG_HEADERS := \
   hbmzip.ch

ifneq ($(HB_HAS_ZLIB),)

   HB_CFLAGS += -DHB_HAS_ZLIB $(foreach d,$(HB_HAS_ZLIB),-I$(d))

   _DET_DSP_NAME := minizip
   _DET_VAR_INC_ := HB_INC_MINIZIP
   _DET_VAR_HAS_ := HB_HAS_MINIZIP
   _DET_FLT_PLAT :=
   _DET_FLT_COMP :=
   _DET_INC_DEFP := /usr/include /usr/local/include
   _DET_INC_LOCL := external/minizip
   _DET_INC_HEAD := /zip.h
   include $(TOP)$(ROOT)config/detfun.mk

   ifneq ($(HB_HAS_MINIZIP),)

      HB_CFLAGS += -DHB_HAS_MINIZIP $(foreach d,$(HB_HAS_MINIZIP),-I$(d))

      include $(TOP)$(ROOT)config/header.mk
      include $(TOP)$(ROOT)config/lib.mk
   else
      HB_SKIP_REASON := $(_DET_RES_TEXT)
      include $(TOP)$(ROOT)config/none.mk
   endif
else
   HB_SKIP_REASON := component not found
   include $(TOP)$(ROOT)config/none.mk
endif
