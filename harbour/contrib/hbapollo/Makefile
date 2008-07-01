#
# $Id$
#

ifeq ($(HB_ARCHITECTURE),w32)

HB_INC_APOLLO_OK += $(foreach d, $(HB_INC_APOLLO), $(if $(wildcard $(d)/sde61.h),$(d),))

ifneq ($(strip $(HB_INC_APOLLO_OK)),)

C_USR += $(foreach d, $(HB_INC_APOLLO_OK), -I$(d))

ROOT = ../../

C_SOURCES=\
	apollo.c \

PRG_SOURCES=\
	apollo1.prg \

PRG_HEADERS=\
	apollo.ch \

LIBNAME=hbapollo

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

endif
endif
