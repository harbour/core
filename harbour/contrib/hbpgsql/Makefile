#
# $Id$
#

ifeq ($(HB_INC_PGSQL),)
HB_INC_PGSQL = /usr/local/pgsql/include /usr/local/pgsql/include/libpq /usr/include/pgsql /usr/include/postgresql /postgres/include
endif

HB_INC_PGSQL_OK += $(foreach d, $(HB_INC_PGSQL), $(if $(wildcard $(d)/libpq-fe.h),$(d),))

ifneq ($(HB_INC_PGSQL_OK),)

C_USR += $(foreach d, $(HB_INC_PGSQL_OK), -I$(d))

ROOT = ../../

C_SOURCES=\
	postgres.c \

PRG_SOURCES=\
	tpostgre.prg \
	pgrdd.prg \

PRG_HEADERS=\
	postgres.ch \

LIBNAME=hbpgsql

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

endif
