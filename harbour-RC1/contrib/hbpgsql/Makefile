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

include $(TOP)$(ROOT)config/lib.cf

PG_DIRS = /usr/local/pgsql/include /usr/local/pgsql/include/libpq \
          /usr/include/pgsql /usr/include/postgresql /postgres/include
CFLAGS += $(foreach d, $(PG_DIRS), $(if $(wildcard $(d)/libpq-fe.h),-I$(d),))
