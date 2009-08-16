#
# $Id$
#

ROOT = ../../

include $(TOP)$(ROOT)config/global.cf

LIBNAME := hbhpdf

ifeq ($(HB_INC_LIBHARU),)
   ifeq ($(HB_XBUILD),)
      HB_INC_LIBHARU = /usr/include /usr/local/include $(TOP)$(ROOT)external/libhpdf
   endif
endif

HB_INC_LIBHARU_OK := $(foreach d, $(HB_INC_LIBHARU), $(if $(wildcard $(d)/hpdf.h),$(d),))

ifneq ($(strip $(HB_INC_LIBHARU_OK)),)

HB_CFLAGS += $(foreach d,$(HB_INC_LIBHARU_OK),-I$(d))

C_SOURCES := \
   harupdf.c \

PRG_HEADERS := \
   harupdf.ch \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
