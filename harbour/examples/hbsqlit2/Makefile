#
# $Id$
#

ROOT = ../../

LIBNAME=hbsqlit2

ifneq ($(HB_ARCHITECTURE),dos)

C_SOURCES =\
	hbsqlit2.c \

PRG_HEADERS=\
	hbsqlit2.ch

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
