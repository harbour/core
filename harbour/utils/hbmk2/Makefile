#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

PRG_SOURCES := \
   hbmk2.prg \

RC_SOURCES := \
   hbmk2.rc

PRG_MAIN := hbmk2.prg

LIBS = $(HB_LIBS_MT_RDD)

ifneq ($(HB_HAS_GPM),)
   HB_PRGFLAGS += -DHB_HAS_GPM
endif
ifneq ($(HB_HAS_WATT),)
   HB_PRGFLAGS += -DHB_HAS_WATT
endif

ifneq ($(filter $(HB_COMPILER),mingw),)
   ifneq ($(HB_BUILD_DYN),no)
      HB_BUILD_SHARED := yes
   endif
endif

include $(TOP)$(ROOT)config/bin.mk
