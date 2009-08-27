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
   freeimage.ch \

_DET_DSP_NAME := FreeImage
_DET_VAR_INC_ := HB_INC_FREEIMAGE
_DET_VAR_HAS_ := HB_HAS_FREEIMAGE
# disabled until a proper solution is found for type collision (except for dos where it's not supported at all)
_DET_FLT_PLAT := !dos !os2 !linux !darwin
_DET_FLT_COMP :=
_DET_INC_DEFP := /usr/include /opt/local/include
_DET_INC_HEAD := /FreeImage.h
include $(TOP)$(ROOT)config/detfun.mk

ifneq ($(HB_HAS_FREEIMAGE),)
   HB_CFLAGS += $(foreach d,$(HB_HAS_FREEIMAGE),-I$(d))

   include $(TOP)$(ROOT)config/header.mk
   INSTALL_RULE_HEADERS := $(INSTALL_RULE)
   include $(TOP)$(ROOT)config/lib.mk

install::
	$(INSTALL_RULE_HEADERS)

else
   HB_SKIP_REASON := $(_DET_RES_TEXT)
   include $(TOP)$(ROOT)config/none.mk
endif
