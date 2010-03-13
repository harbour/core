#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

PRG_SOURCES := \
   hbrun.prg \

PRG_MAIN := hbrun.prg

LIBS = hbcplr hbpp hbcommon $(HB_LIBS_ST_RDD)

ifeq ($(_DEFAULT_INC_DIR),)
   ifneq ($(HB_INC_INSTALL),)
      ifneq ($(HB_INST_PKGPREF),)
         _DEFAULT_INC_DIR := $(HB_INC_INSTALL)
      endif
   endif
endif
ifneq ($(_DEFAULT_INC_DIR),)
   HB_PRGFLAGS += -D_DEFAULT_INC_DIR=[$(_DEFAULT_INC_DIR)]
endif

include $(TOP)$(ROOT)config/bin.mk
