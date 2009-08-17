#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtsln

C_SOURCES := \
   gtsln.c \
   kbsln.c \
   mousesln.c \

ifeq ($(HB_INC_SLANG),)
   ifeq ($(HB_XBUILD),)
      ifeq ($(HB_LOCAL_SLN),yes)
         HB_INC_SLANG := /usr/local/include /usr/local/include/slang
         LDFLAGS := -L/usr/local/lib $(LDFLAGS)
      else
         HB_INC_SLANG := /usr/include /usr/include/slang /usr/local/include /usr/local/include/slang /sw/include /sw/include/slang /opt/local/include /opt/local/include/slang
         LDFLAGS := -L/usr/lib/slang -L/usr/local/lib -L/usr/local/lib/slang $(LDFLAGS)
      endif
   endif
endif

ifeq ($(HB_WITHOUT_GTSLN),yes)
   HB_INC_SLANG :=
endif
ifeq ($(HB_COMMERCE),yes)
   HB_INC_SLANG :=
endif

HB_INC_SLANG_OK := $(foreach d, $(HB_INC_SLANG), $(if $(wildcard $(d)/slang.h),$(d),))

ifneq ($(strip $(HB_INC_SLANG_OK)),)

ifeq ($(HB_GPM_MOUSE),yes)
   HB_CFLAGS += -DHB_HAS_GPM
   ifeq ($(HB_GPM_NOICE_DISABLE),yes)
      HB_CFLAGS += -DHB_GPM_NOICE_DISABLE
   endif
endif

HB_CFLAGS += $(foreach d,$(HB_INC_SLANG_OK),-I$(d))

include $(TOP)$(ROOT)config/lib.mk

else
include $(TOP)$(ROOT)config/none.mk
endif
