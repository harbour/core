#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.cf

LIBNAME := hbblat

ifeq ($(HB_ARCHITECTURE),win)

C_SOURCES := \
	blatwrp.c \

PRG_SOURCES := \
	blatcls.prg \

PRG_HEADERS := \
	hbblat.ch \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
