#
# $Id$
#

# Needs hbw32 and hbodbc, but these won't build with rsxnt
ifneq ($(HB_COMPILER),rsxnt)

ROOT = ../../

PRG_SOURCES= \
	adordd.prg \

PRG_HEADERS=\
	adordd.ch \

LIBNAME=rddado

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

endif
