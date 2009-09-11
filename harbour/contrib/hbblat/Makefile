#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbblat

C_SOURCES := \
   blatwrp.c \

PRG_SOURCES := \
   blatcls.prg \

PRG_HEADERS := \
   hbblat.ch \

ifeq ($(HB_PLATFORM),win)
   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := platform not supported
   include $(TOP)$(ROOT)config/none.mk
endif
