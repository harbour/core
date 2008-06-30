#
# $Id$
#

ROOT = ../../

C_SOURCES=\
	postgres.c \

PRG_SOURCES=\
	tpostgre.prg \
	pgrdd.prg \

PRG_HEADERS=\
	postgres.ch \

LIBNAME=hbpgsql

PG_DIRS = /usr/local/pgsql/include /usr/local/pgsql/include/libpq \
          /usr/include/pgsql /usr/include/postgresql /postgres/include
C_USR += $(foreach d, $(PG_DIRS), $(if $(wildcard $(d)/libpq-fe.h),-I$(d),))

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
