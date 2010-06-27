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
   HB_BUILD_SHARED := yes
endif

include $(TOP)$(ROOT)config/bin.mk
