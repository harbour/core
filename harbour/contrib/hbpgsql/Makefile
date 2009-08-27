#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbpgsql

C_SOURCES := \
   postgres.c \

PRG_SOURCES := \
   tpostgre.prg \
   pgrdd.prg \

PRG_HEADERS := \
   postgres.ch \

_DET_DSP_NAME := postgresql
_DET_VAR_INC_ := HB_INC_PGSQL
_DET_VAR_HAS_ := HB_HAS_PGSQL
_DET_FLT_PLAT :=
_DET_FLT_COMP :=
_DET_INC_DEFP := /usr/include /usr/local/pgsql/include /usr/local/pgsql/include/libpq
_DET_INC_DEFP += /usr/include/pgsql /usr/include/postgresql /postgres/include /opt/local/include/postgresql83
_DET_INC_HEAD := /libpq-fe.h
include $(TOP)$(ROOT)config/detfun.mk

ifneq ($(HB_HAS_PGSQL),)

   HB_CFLAGS += $(foreach d,$(HB_HAS_PGSQL),-I$(d))

   include $(TOP)$(ROOT)config/header.mk
   INSTALL_RULE_HEADERS := $(INSTALL_RULE)
   include $(TOP)$(ROOT)config/lib.mk

   install::
	$(INSTALL_RULE_HEADERS)

else
   HB_SKIP_REASON := $(_DET_RES_TEXT)
   include $(TOP)$(ROOT)config/none.mk
endif
