#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbfbird

C_SOURCES := \
   firebird.c \

PRG_SOURCES := \
   tfirebrd.prg \

ifeq ($(filter $(HB_COMPILER),pocc64 poccarm),)

   ifeq ($(HB_INC_FIREBIRD),)
      ifeq ($(HB_XBUILD),)
         HB_INC_FIREBIRD := /usr/include /opt/firebird/include
      endif
   endif

   HB_INC_FIREBIRD_OK := $(foreach d, $(HB_INC_FIREBIRD), $(if $(wildcard $(d)/ibase.h),$(d),))

   ifneq ($(strip $(HB_INC_FIREBIRD_OK)),)

      HB_CFLAGS += $(foreach d,$(HB_INC_FIREBIRD_OK),-I$(d))

      include $(TOP)$(ROOT)config/lib.mk
   else
      include $(TOP)$(ROOT)config/none.mk
   endif
else
   include $(TOP)$(ROOT)config/none.mk
endif
