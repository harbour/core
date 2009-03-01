#
# $Id$
#

ROOT = ../../

LIBNAME=xhb

C_SOURCES=\
	dbf2txt.c \
	filestat.c \
	freadlin.c \
	hbcrypt.c \
	hbxml.c \
	hbsyslog.c \
	hboutdbg.c \
	cstructc.c \
	xhbarr.c \
	xhbcopyf.c \
	xhbenum.c \
	xhbfunc.c \
	xhbmsgs.c \
	xhbqself.c \
	xhbwith.c \
	hbcomprs.c \
	xstrdel.c \
	dbgfxc.c \

C_HEADERS=\
	hbcompat.h \
	xhb.h \

PRG_SOURCES=\
	arrayblk.prg \
	txml.prg \
	hblog.prg \
	hblognet.prg \
	cstruct.prg \
	hbstruct.prg \
	dirrec.prg \
	xhbcomp.prg \
	dbgfx.prg \
	sprintf.prg \
	regexrpl.prg \

PRG_HEADERS=\
	hbcompat.ch \
	xhb.ch \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
