#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := sddmy

C_SOURCES := \
   mysqldd.c \

_DET_DSP_NAME := mysql
_DET_VAR_INC_ := HB_INC_MYSQL
_DET_VAR_HAS_ := HB_HAS_MYSQL
_DET_FLT_PLAT :=
_DET_FLT_COMP :=
_DET_INC_DEFP := /usr/include/mysql /opt/local/include/mysql5/mysql
_DET_INC_HEAD := /mysql.h
include $(TOP)$(ROOT)config/detfun.mk

ifneq ($(HB_HAS_MYSQL),)

   HB_CFLAGS += $(foreach d,$(HB_HAS_MYSQL),-I$(d))

   include $(TOP)$(ROOT)config/header.mk
   INSTALL_RULE_HEADERS := $(INSTALL_RULE)
   include $(TOP)$(ROOT)config/lib.mk

   install::
	$(INSTALL_RULE_HEADERS)

else
   HB_SKIP_REASON := $(_DET_RES_TEXT)
   include $(TOP)$(ROOT)config/none.mk
endif
