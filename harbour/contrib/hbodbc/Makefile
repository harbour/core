#
# $Id$
#

ifneq ($(HB_WITHOUT_ODBC),yes)
ifneq ($(HB_COMPILER),rsxnt)

ROOT = ../../

C_SOURCES= \
	odbc.c \

PRG_SOURCES= \
	todbc.prg \
	browodbc.prg \

PRG_HEADERS=\
	sql.ch \

LIBNAME=hbodbc

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

endif
endif
