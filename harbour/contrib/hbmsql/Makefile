#
# $Id$
#

ROOT = ../../

C_SOURCES=\
	msql.c \

PRG_SOURCES=\
	tmsql.prg \

PRG_HEADERS=\
	msql.ch \

LIBNAME=hbmsql

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
