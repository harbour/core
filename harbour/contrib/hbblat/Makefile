#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbblat

ifeq ($(HB_PLATFORM),win)

C_SOURCES := \
   blatwrp.c \

PRG_SOURCES := \
   blatcls.prg \

PRG_HEADERS := \
   hbblat.ch \

include $(TOP)$(ROOT)config/header.mk
include $(TOP)$(ROOT)config/lib.mk

else
include $(TOP)$(ROOT)config/none.mk
endif
