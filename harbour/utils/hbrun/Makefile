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

ifneq ($(HB_INSTALL_INC),)
   HB_PRGFLAGS += -D_HBRUN_HB_INSTALL_INC=[$(HB_INSTALL_INC)]
endif

include $(TOP)$(ROOT)config/bin.mk
