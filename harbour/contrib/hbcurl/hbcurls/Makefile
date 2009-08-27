#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

vpath %.c ../

LIBNAME := hbcurls

C_SOURCES := \
   hbcurl.c \

_DET_DSP_NAME := libcurl
_DET_VAR_INC_ := HB_INC_CURL
_DET_VAR_HAS_ := HB_HAS_CURL
_DET_FLT_PLAT := !dos
_DET_FLT_COMP :=
_DET_INC_DEFP := /usr/include
_DET_INC_HEAD := /curl/curl.h
include $(TOP)$(ROOT)config/detfun.mk

ifneq ($(HB_HAS_CURL),)

   HB_CFLAGS += $(foreach d,$(HB_HAS_CURL),-I$(d))
   # always export symbols
   HB_CFLAGS += -DCURL_STATICLIB

   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := $(_DET_RES_TEXT)
   include $(TOP)$(ROOT)config/none.mk
endif
