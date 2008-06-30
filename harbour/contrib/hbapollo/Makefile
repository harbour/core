#
# $Id$
#

ifeq ($(HB_ARCHITECTURE),w32)
ifneq ($(HB_INC_APOLLO),)

C_USR += -I"${HB_INC_APOLLO}"

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
