#
# $Id$
#

ROOT = ../../

YACC_SOURCE=harbour.y

YACC_HEADERS=\
	compiler.h \
	hbsetup.h \
	pcode.h \
	hbdefs.h \
	hberrors.h \
	hbpp.h

LEX_SOURCE=harbour.l

LEX_HEADERS=\
	hbsetup.h \
	hberrors.h \
	hbdefs.h

C_SOURCES=\
        genc.c \
        genhrb.c \
        genjava.c \
        genobj32.c \
        genpas.c \
        genrc.c \

C_MAIN=harbour.c

LIBS=\
	hbpp \

include $(TOP)$(ROOT)config/bin.cf
