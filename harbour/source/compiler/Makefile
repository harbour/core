#
# $Id$
#

ROOT = ../../

YACC_SOURCE=harbour.y

YACC_HEADERS=\
	hbcomp.h \
	hbsetup.h \
	hbpcode.h \
	hbdefs.h \
	hberrors.h \
	hbpp.h

LEX_SOURCE=harbour.l

LEX_HEADERS=\
	hbsetup.h \
	hberrors.h \
	hbdefs.h

C_SOURCES=\
	cmdcheck.c \
	genc.c \
	genhrb.c \
	genjava.c \
	genobj32.c \
	hbfunchk.c \
	hbgenerr.c \
	hbpcode.c \
	hbusage.c \
	hbfix.c \
	expropta.c \
	exproptb.c \
	exproptc.c \

C_MAIN=harbour.c

LIBS=\
	pp \
	common \

include $(TOP)$(ROOT)config/bin.cf
