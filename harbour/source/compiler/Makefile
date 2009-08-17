#
# $Id$
#

ROOT := ../../

#LEX_FLAGS := -Phb_comp -C
#LEX_SOURCE := harbour.l
#LEX_HEADERS := \
#   hbsetup.h \
#   hberrors.h \
#   hbdefs.h \

#YACC_FLAGS := -p hb_comp
YACC_SOURCE := harbour.y
YACC_DEPEND := complex.c
YACC_HEADERS := \
   hbcomp.h \
   hbcompdf.h \
   hbsetup.h \
   hbpcode.h \
   hbdefs.h \
   hberrors.h \
   hbpp.h \

C_SOURCES := \
   cmdcheck.c \
   genc.c \
   gencc.c \
   gencobj.c \
   genhrb.c \
   hbcmplib.c \
   hbcomp.c \
   hbdbginf.c \
   hbfunchk.c \
   hbgenerr.c \
   hbpcode.c \
   hbfix.c \
   hbdead.c \
   hblbl.c \
   hbopt.c \
   hbstripl.c \
   hbusage.c \
   hbident.c \
   ppcomp.c \
   expropta.c \
   exproptb.c \
   complex.c \
   compi18n.c \
   hbmain.c \

LIBNAME := hbcplr

include $(TOP)$(ROOT)config/lib.mk
