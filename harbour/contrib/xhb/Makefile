#
# $Id$
#

ROOT = ../../

C_SOURCES=\
	hbxml.c \
	hbsyslog.c \
	hboutdbg.c \
	cstructc.c \
	xhbenum.c \
	xhbfunc.c \
	xhbmsgs.c \
	xhbqself.c \
	xhbwith.c \
	hbcomprs.c \
	hbchksum.c \
	xstrdel.c \

C_HEADERS=\
	hbcompat.h \
	hbcomprs.h \
	hbfast.h \
	hbxml.h \

PRG_SOURCES=\
	txml.prg \
	hblog.prg \
	hblognet.prg \
	cstruct.prg \
	xhbcomp.prg \

PRG_HEADERS=\
	classex.ch \
	cstruct.ch \
	hbcompat.ch \
	hbctypes.ch \
	hblog.ch \
	hblogdef.ch \
	math.ch \
	usrrdd.ch \
	wintypes.ch \
	xhb.ch \
	xhbextrn.ch \

LIBNAME=xhb

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
