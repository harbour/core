#
# $Id$
#

HB_USER_CFLAGS += -I$(TOP)$(ROOT)contrib/hbwin

ROOT = ../../

LIBNAME=gtwvg

ifeq ($(HB_ARCHITECTURE),win)

C_SOURCES=\
	gtwvg.c \
	wvgcore.c \
	wvgutils.c \
	wvgwin.c \
	wvgsink.c \
	wvggui.c \
	wincallb.c \
	wvgcuig.c \

PRG_SOURCES=\
	wvgclass.prg \
	wvgpaint.prg \
	wvgphdlr.prg \
	wvgwnd.prg \
	wvgcrt.prg \
	wvgax.prg \
	wvgdlg.prg \
	wvgmenub.prg \
	wvgtoolb.prg \
	wvgcombo.prg \
	wvglistb.prg \
	wvgpushb.prg \
	wvgstatb.prg \
	wvgstatc.prg \
	wvgtreev.prg \
	wincback.prg \
	wvgcheck.prg \
	wvgdatar.prg \
	wvgradio.prg \
	wvg3stat.prg \
	wvgbitmp.prg \
	wvgsle.prg \
	wvgmle.prg \
	wvghtmlv.prg \
	wvgsysw.prg \
	wvgdarea.prg \
	wvgscrlb.prg \

PRG_HEADERS=\
	hbgtwvg.ch \
	wvtwin.ch \
	wvgparts.ch \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
