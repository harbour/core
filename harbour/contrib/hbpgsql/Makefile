#
# $Id$
#

ROOT := ../../

LIBNAME := hbpgsql

ifeq ($(HB_INC_PGSQL),)
ifeq ($(HB_XBUILD),)
HB_INC_PGSQL := /usr/include /usr/local/pgsql/include /usr/local/pgsql/include/libpq /usr/include/pgsql /usr/include/postgresql /postgres/include /opt/local/include/postgresql83
endif
endif

HB_INC_PGSQL_OK += $(foreach d, $(HB_INC_PGSQL), $(if $(wildcard $(d)/libpq-fe.h),$(d),))

ifneq ($(strip $(HB_INC_PGSQL_OK)),)

HB_USER_CFLAGS += $(foreach d, $(HB_INC_PGSQL_OK), -I$(d))

C_SOURCES := \
   postgres.c \

PRG_SOURCES := \
   tpostgre.prg \
   pgrdd.prg \

PRG_HEADERS := \
   postgres.ch \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
