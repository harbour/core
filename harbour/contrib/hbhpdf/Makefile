#
# $Id$
#

ifeq ($(HB_ARCHITECTURE),linux)
ifeq ($(HB_INC_LIBHARU),)
HB_INC_LIBHARU = /usr/include
endif
endif

HB_INC_LIBHARU_OK += $(foreach d, $(HB_INC_LIBHARU), $(if $(wildcard $(d)/hpdf.h),$(d),))

ifneq ($(HB_INC_LIBHARU_OK),)

C_USR += $(foreach d, $(HB_INC_LIBHARU_OK), -I$(d))

ROOT = ../../

C_SOURCES=\
	harupdf.c \

PRG_HEADERS=\
	harupdf.ch \

PRG_SOURCES=\

LIBNAME=hbhpdf

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

endif
