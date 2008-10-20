#
# $Id: Makefile 7358 2007-05-09 08:08:49Z lf_sfnet $
#

ROOT = ../../

C_SOURCES=\
	dbgfxc.c \

C_HEADERS=\
	hbdbgfx.h \

PRG_SOURCES=\
	dbgfx.prg \
	sprintf.prg \

LIBNAME=hbdbgfx

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
