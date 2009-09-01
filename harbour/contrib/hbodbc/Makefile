#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbodbc

C_SOURCES := \
   odbc.c \
   odbcold.c \

PRG_SOURCES := \
   todbc.prg \
   browodbc.prg \

PRG_HEADERS := \
   sql.ch \

ifneq ($(filter $(HB_PLATFORM),win wce),)
   HB_INC_ODBC := force
endif

_DET_DSP_NAME := odbc
_DET_VAR_INC_ := HB_INC_ODBC
_DET_VAR_HAS_ := HB_HAS_ODBC
_DET_FLT_PLAT := !dos !os2
_DET_FLT_COMP := !rsxnt
_DET_INC_DEFP := /usr/include /usr/local/include
_DET_INC_HEAD := /sql.h
include $(TOP)$(ROOT)config/detfun.mk

ifneq ($(HB_HAS_ODBC),)

   HB_CFLAGS += $(foreach d,$(HB_HAS_ODBC),-I$(d))

   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk

else
   HB_SKIP_REASON := $(_DET_RES_TEXT)
   include $(TOP)$(ROOT)config/none.mk
endif
