#
# $Id$
#

ROOT = ../../

LIBNAME=hbw32

ifeq ($(HB_ARCHITECTURE),w32)

C_SOURCES=\
	win_dll.c \
	win_ole.c \
	win_osc.c \
	win_prn1.c \
	win_prn2.c \
	win_prt.c \
	win_regc.c \

PRG_SOURCES=\
	win_os.prg \
	win_reg.prg \
	win_tole.prg \
	win_tprn.prg \
	win_tprt.prg \

C_HEADERS = \
	hbwin.h \

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
