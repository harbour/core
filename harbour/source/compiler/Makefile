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

ifeq ($(HB_LEX),SIMPLEX)

C_EXTRA=hbslex.c

else

LEX_SOURCE=harbour.l

LEX_HEADERS=\
	hbsetup.h \
	hberrors.h \
	hbdefs.h
endif

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
	hbident.c \
	expropta.c \
	exproptb.c \
	exproptc.c \
	$(C_EXTRA)

C_MAIN=harbour.c

LIBS=\
	pp \
	common \

include $(TOP)$(ROOT)config/bin.cf
