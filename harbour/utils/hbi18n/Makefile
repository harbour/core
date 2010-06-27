#
# $Id$
#

ROOT := ../../

PRG_SOURCES := \
   hbi18n.prg \

PRG_MAIN := hbi18n.prg

LIBS = $(HB_LIBS_ST_NORDD)

ifneq ($(filter $(HB_PLATFORM),win wce os2),)
   HB_BUILD_SHARED := yes
endif

include $(TOP)$(ROOT)config/bin.mk
