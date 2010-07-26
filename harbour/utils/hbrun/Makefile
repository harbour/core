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

ifneq ($(HB_INC_INSTALL),)
   HB_PRGFLAGS += -D_HBRUN_HB_INC_INSTALL=[$(HB_INC_INSTALL)]
endif

include $(TOP)$(ROOT)config/bin.mk
