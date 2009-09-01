#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := rddads

C_SOURCES := \
   ads1.c \
   adsfunc.c \
   adsmgmnt.c \
   adsx.c \

PRG_HEADERS := \
   ads.ch \
   adsextrn.ch \

_DET_DSP_NAME := ads
_DET_VAR_INC_ := HB_INC_ADS
_DET_VAR_HAS_ := HB_HAS_ADS
_DET_FLT_PLAT := !dos !os2
_DET_FLT_COMP :=
_DET_INC_DEFP := /usr/local/ads/acesdk $(HOME)/ads/acesdk
_DET_INC_HEAD := /ace.h
include $(TOP)$(ROOT)config/detfun.mk

ifneq ($(HB_HAS_ADS),)

   HB_CFLAGS += $(foreach d,$(HB_HAS_ADS),-I$(d))

   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk

else
   HB_SKIP_REASON := $(_DET_RES_TEXT)
   include $(TOP)$(ROOT)config/none.mk
endif
