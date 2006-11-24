#
# $Id$
#

ROOT = ../../

ifeq ($(HB_LEX),SIMPLEX)

YACC_SOURCE=harbour.sly

else

#YACC_FLAGS = -p hb_comp
YACC_SOURCE=harbour.y

endif

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

#LEX_SOURCE=harbour.l
C_EXTRA=complex.c

LEX_HEADERS=\
    hbsetup.h \
    hberrors.h \
    hbdefs.h
endif

C_SOURCES=\
    cmdcheck.c \
    genc.c     \
    gencc.c    \
    gencobj.c  \
    genhrb.c   \
    genjava.c  \
    genobj32.c \
    gencli.c   \
    hbcomp.c   \
    hbfunchk.c \
    hbgenerr.c \
    hbpcode.c  \
    hbfix.c    \
    hbdead.c   \
    hblbl.c    \
    hbstripl.c \
    hbusage.c  \
    hbident.c  \
    ppcomp.c   \
    expropta.c \
    exproptb.c \
    exproptc.c \
    $(C_EXTRA)


C_MAIN=harbour.c

LIBS=\
    pp \
    common \

include $(TOP)$(ROOT)config/bin.cf
