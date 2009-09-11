#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := sddpg

C_SOURCES := \
   pgsqldd.c \

_DET_DSP_NAME := postgresql
_DET_VAR_INC_ := HB_INC_PGSQL
_DET_VAR_HAS_ := HB_HAS_PGSQL
_DET_FLT_PLAT :=
_DET_FLT_COMP :=
_DET_INC_DEFP := /usr/include /usr/local/pgsql/include /usr/local/pgsql/include/libpq
_DET_INC_DEFP += /usr/include/pgsql /usr/include/postgresql /postgres/include /opt/local/include/postgresql84 /opt/local/include/postgresql83
_DET_INC_HEAD := /libpq-fe.h
include $(TOP)$(ROOT)config/detfun.mk

ifneq ($(HB_HAS_PGSQL),)
   HB_CFLAGS += $(foreach d,$(HB_HAS_PGSQL),-I$(d))
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := $(_DET_RES_TEXT)
   include $(TOP)$(ROOT)config/none.mk
endif
