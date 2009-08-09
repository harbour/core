#
# $Id$
#

ROOT := ../../../

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

HB_INC_SLANG_OK += $(foreach d, $(HB_INC_SLANG), $(if $(wildcard $(d)/slang.h),$(d),))

ifneq ($(strip $(HB_INC_SLANG_OK)),)

HB_INC_GPM_OK :=
ifneq ($(HB_GPM_MOUSE),no)
ifneq ($(HB_COMMERCE),yes)
HB_INC_GPM := /usr/include /usr/local/include
HB_INC_GPM_OK += $(foreach d, $(HB_INC_GPM), $(if $(wildcard $(d)/gpm.h),$(d),))
endif
endif

ifneq ($(strip $(HB_INC_GPM_OK)),)
  HB_USER_CFLAGS += -DHAVE_GPM_H
ifeq ($(HB_GPM_NOICE_DISABLE),yes)
  HB_USER_CFLAGS += -DHB_GPM_NOICE_DISABLE
endif
endif

HB_USER_CFLAGS += $(foreach d, $(HB_INC_SLANG_OK), -I$(d))

include $(TOP)$(ROOT)config/lib.cf

else
include $(TOP)$(ROOT)config/none.cf
endif
