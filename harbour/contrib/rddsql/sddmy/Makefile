#
# $Id$
#

ROOT := ../../../

LIBNAME := sddmy

ifeq ($(HB_INC_MYSQL),)
ifeq ($(HB_XBUILD),)
HB_INC_MYSQL := /usr/include/mysql /opt/local/include/mysql5/mysql
endif
endif

HB_INC_MYSQL_OK := $(foreach d, $(HB_INC_MYSQL), $(if $(wildcard $(d)/mysql.h),$(d),))

ifneq ($(strip $(HB_INC_MYSQL_OK)),)

HB_USER_CFLAGS += $(foreach d, $(HB_INC_MYSQL_OK), -I$(d))

C_SOURCES := \
   mysqldd.c \

include $(TOP)$(ROOT)config/lib.cf
else
include $(TOP)$(ROOT)config/none.cf
endif
