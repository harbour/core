#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtalleg

ifeq ($(filter $(HB_COMPILER),pocc pocc64 poccarm xcc dmc watcom mingwarm),)

ifeq ($(HB_INC_ALLEGRO),)
   ifeq ($(HB_XBUILD),)
      HB_INC_ALLEGRO := /usr/include /opt/local/include
   endif
endif

HB_INC_ALLEGRO_OK := $(foreach d, $(HB_INC_ALLEGRO), $(if $(wildcard $(d)/allegro.h),$(d),))

ifneq ($(strip $(HB_INC_ALLEGRO_OK)),)

ifneq ($(HB_ALLEGRO_STATIC),)
   LIBNAME := gtallegs
   HB_CFLAGS += -DALLEGRO_STATICLINK
endif

HB_CFLAGS += $(foreach d,$(HB_INC_ALLEGRO_OK),-I$(d))

C_SOURCES := \
   gtalleg.c \
   ssf.c \

include $(TOP)$(ROOT)config/lib.mk

else
HB_SKIP_REASON := dependency not set or not found
include $(TOP)$(ROOT)config/none.mk
endif
else
HB_SKIP_REASON := not supported
include $(TOP)$(ROOT)config/none.mk
endif
