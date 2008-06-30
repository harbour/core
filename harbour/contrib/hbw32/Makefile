#
# $Id$
#

ifeq ($(HB_ARCHITECTURE),w32)
ifneq ($(HB_COMPILER),rsxnt)

ROOT = ../../

C_SOURCES=\
	dllcall.c \
	tprinter.c \
	w32_ole.c \
	w32_prn.c \
	w32_regc.c \

PRG_SOURCES=\
	w32_tole.prg \
	w32_tprn.prg \
	w32_reg.prg \

PRG_HEADERS = \
	hbw32.ch \

LIBNAME=hbw32

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

endif
endif
