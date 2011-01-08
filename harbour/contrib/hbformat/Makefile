#
# $Id$
#

ROOT := ../../

PRG_SOURCES := \
   hbformat.prg \

C_SOURCES := \
   hbformac.c \

PRG_MAIN := hbformat.prg

LIBS = $(HB_LIBS_ST_NORDD)

ifneq ($(filter $(HB_PLATFORM),win wce os2),)
   ifneq ($(HB_BUILD_DYN),no)
      HB_BUILD_SHARED := yes
   endif
endif

include $(TOP)$(ROOT)config/bin.mk
