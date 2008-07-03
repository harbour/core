#
# $Id$
#

ROOT = ../../

LIBNAME=hbsqlit3

ifneq ($(HB_ARCHITECTURE),dos)
ifneq ($(HB_ARCHITECTURE),bsd)
ifneq ($(HB_ARCHITECTURE),hpux)

C_SOURCES =\
	hbsqlit3.c \

PRG_HEADERS=\
	hbsqlit3.ch \

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
