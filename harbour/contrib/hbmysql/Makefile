#
# $Id$
#

ifeq ($(HB_ARCHITECTURE),linux)
ifeq ($(HB_INC_MYSQL),)
# a workaround of a problem with different include directories 
# for mysql header files between different unix distributions
HB_INC_MYSQL = /usr/include/mysql
endif
endif

HB_INC_MYSQL_OK += $(foreach d, $(HB_INC_MYSQL), $(if $(wildcard $(d)/mysql.h),$(d),))

ifneq ($(HB_INC_MYSQL_OK),)

C_USR += $(foreach d, $(HB_INC_MYSQL_OK), -I$(d))

ROOT = ../../

C_SOURCES=\
	mysql.c \

PRG_SOURCES=\
	tmysql.prg \
	tsqlbrw.prg \

PRG_HEADERS=\
	mysql.ch \

LIBNAME=hbmysql

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

endif
