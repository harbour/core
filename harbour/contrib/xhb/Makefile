#
# $Id$
#

ROOT = ../../

C_SOURCES=\
	hbxml.c    \
	hbsyslog.c \
	hboutdbg.c \
	cstructc.c \
	xhbenum.c  \
	xhbfunc.c  \
	xhbmsgs.c  \
	xhbwith.c  \

PRG_SOURCES=\
	txml.prg     \
	hblog.prg    \
	hblognet.prg \
	cstruct.prg  \
	xhbcomp.prg  \

PRG_HEADERS=\
	xhb.ch      \
	hbcompat.ch \
	hblog.ch    \
	hblogdef.ch \
	cstruct.ch  \
	hbctypes.ch \
	wintypes.ch \
	xhbextrn.ch \

LIBNAME=xhb

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
