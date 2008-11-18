#
# $Id$
#

ROOT = ../../

LIBNAME=gtwvg

ifeq ($(HB_ARCHITECTURE),w32)

C_SOURCES=\
	gtwvg.c \
	wvgcore.c \
	wvgutils.c \
	wvgwin.c \
	wvgsink.c \
	wvggui.c \

PRG_SOURCES=\
	wvgclass.prg \
	wvgpaint.prg \
	wvgphdlr.prg \
	wvgwnd.prg \
	wvgcrt.prg \
	wvgax.prg \
	wvgdlg.prg \

PRG_HEADERS=\
	hbgtwvg.ch \
	wvtwin.ch \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
