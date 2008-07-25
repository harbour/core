#
# $Id$
#

ROOT = ../../

C_SOURCES=\
	freadlin.c \
	hbcrypt.c \
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
	xstrdel.c \

C_HEADERS=\
	hbcompat.h \

PRG_SOURCES=\
	txml.prg \
	hblog.prg \
	hblognet.prg \
	cstruct.prg \
	dirrec.prg \
	xhbcomp.prg \

PRG_HEADERS=\
	hbcompat.ch \
	xhb.ch \

LIBNAME=xhb

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
