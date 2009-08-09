#
# $Id$
#

ROOT := ../../

LIBNAME := rddado

ifneq ($(HB_ARCHITECTURE),os2)

PRG_SOURCES := \
	adordd.prg \

PRG_HEADERS := \
	adordd.ch \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
