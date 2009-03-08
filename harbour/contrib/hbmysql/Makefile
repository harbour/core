#
# $Id$
#

ROOT = ../../

LIBNAME=hbmysql

ifeq ($(HB_INC_MYSQL),)
ifeq ($(HB_XBUILD),)
HB_INC_MYSQL = /usr/include/mysql /opt/local/include/mysql5/mysql
endif
endif

HB_INC_MYSQL_OK += $(foreach d, $(HB_INC_MYSQL), $(if $(wildcard $(d)/mysql.h),$(d),))

ifneq ($(strip $(HB_INC_MYSQL_OK)),)

HB_USER_CFLAGS += $(foreach d, $(HB_INC_MYSQL_OK), -I$(d))

C_SOURCES=\
	mysql.c \

PRG_SOURCES=\
	tmysql.prg \
	tsqlbrw.prg \

PRG_HEADERS=\
	mysql.ch \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
