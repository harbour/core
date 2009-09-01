#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := rddado

ifneq ($(HB_PLATFORM),os2)

PRG_SOURCES := \
   adordd.prg \

PRG_HEADERS := \
   adordd.ch \

include $(TOP)$(ROOT)config/header.mk
include $(TOP)$(ROOT)config/lib.mk

else
include $(TOP)$(ROOT)config/none.mk
endif
