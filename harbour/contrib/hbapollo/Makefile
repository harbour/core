#
# $Id$
#

ROOT = ../../

LIBNAME=hbapollo

ifneq ($(HB_ARCHITECTURE),dos)
ifneq ($(HB_COMPILER),mingw64)

HB_INC_APOLLO_OK += $(foreach d, $(HB_INC_APOLLO), $(if $(wildcard $(d)/Sde61.h),$(d),))

ifneq ($(strip $(HB_INC_APOLLO_OK)),)

HB_USER_CFLAGS += $(foreach d, $(HB_INC_APOLLO_OK), -I$(d))

C_SOURCES=\
	apollo.c \

PRG_SOURCES=\
	apollo1.prg \

PRG_HEADERS=\
	apollo.ch \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
else
include $(TOP)$(ROOT)config/none.cf
endif
else
include $(TOP)$(ROOT)config/none.cf
endif
