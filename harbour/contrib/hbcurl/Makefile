#
# $Id$
#

ROOT = ../../

C_SOURCES=\
	hbcurl.c \

PRG_HEADERS=\
	hbcurl.ch \

LIBNAME=hbcurl

C_USR += -I/usr/include/curl

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
