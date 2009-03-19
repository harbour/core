#
# $Id$
#

ROOT = ../../

LIBNAME=xhb

C_SOURCES=\
	cstructc.c \
	dbf2txt.c \
	dbgfxc.c \
	filestat.c \
	freadlin.c \
	hbcomprs.c \
	hbcrypt.c \
	hboutdbg.c \
	hbserv.c \
	hbsyslog.c \
	hbxml.c \
	xhbarr.c \
	xhbcopyf.c \
	xhbenum.c \
	xhbfunc.c \
	xhbmsgs.c \
	xhbqself.c \
	xhbwith.c \
	xstrdel.c \

C_HEADERS=\
	hbcompat.h \
	xhb.h \

PRG_SOURCES=\
	arrayblk.prg \
	cstruct.prg \
	dbgfx.prg \
	dirrec.prg \
	hblog.prg \
	hblognet.prg \
	hbstruct.prg \
	regexrpl.prg \
	sprintf.prg \
	trpc.prg \
	trpccli.prg \
	ttable.prg \
	txml.prg \
	xhbcomp.prg \
	xhbmt.prg \

PRG_HEADERS=\
	hbcompat.ch \
	xhb.ch \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
