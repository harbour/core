#
# $Id$
#

ROOT = ../../

PRG_SOURCES=\
	hbvpdf.prg \
	hbvpdft.prg \
	hbvpsup.prg \

PRG_HEADERS=\
	hbvpdf.ch \

LIBNAME=hbvpdf

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
