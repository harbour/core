#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.cf

LIBNAME := hbtpathy

C_SOURCES := \
	tpcommon.c \
	tpunix.c \
	tpwin.c \
	tpos2.c \
	tpdos.c \

PRG_SOURCES := \
	telepath.prg \

PRG_HEADERS := \
	telepath.ch \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
