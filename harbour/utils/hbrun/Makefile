#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

PRG_SOURCES := \
   hbrun.prg \

RC_SOURCES := \
   hbrun.rc

PRG_MAIN := hbrun.prg

LIBS = hbcplr hbpp hbcommon $(HB_LIBS_MT_RDD)

HB_PRGFLAGS += -DHBRUN_WITH_HEADERS

include $(TOP)$(ROOT)config/bin.mk
