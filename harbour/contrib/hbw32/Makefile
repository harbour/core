#
# $Id$
#

ROOT = ../../

LIBNAME=hbw32

ifeq ($(HB_ARCHITECTURE),w32)

C_SOURCES=\
	dllcall.c \
	tprinter.c \
	w32_ole.c \
	w32_osc.c \
	w32_prn.c \
	w32_regc.c \

PRG_SOURCES=\
	w32_os.prg \
	w32_reg.prg \
	w32_tole.prg \
	w32_tprn.prg \

PRG_HEADERS = \
	hbw32.ch \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
