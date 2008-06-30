#
# $Id$
#

ROOT = ../../

C_SOURCES=\
	harupdf.c \

PRG_HEADERS=\
	harupdf.ch \

PRG_SOURCES=\

LIBNAME=hbhpdf

C_USR += -I/usr/include

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
