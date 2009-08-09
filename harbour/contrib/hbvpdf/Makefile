#
# $Id$
#

ROOT := ../../

LIBNAME := hbvpdf

PRG_SOURCES := \
	hbvpdf.prg \
	hbvpdft.prg \
	hbvpsup.prg \

PRG_HEADERS := \
	hbvpdf.ch \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
