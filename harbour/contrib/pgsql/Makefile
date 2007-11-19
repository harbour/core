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

LIBNAME=hbpg

include $(TOP)$(ROOT)config/lib.cf

ifeq ($(HB_COMPILER),mingw32)
   CFLAGS := $(CFLAGS) -I/postgres/include
else
   CFLAGS := $(CFLAGS) -I/usr/local/pgsql/include -I/usr/local/pgsql/include/libpq -I/usr/include/pgsql
endif
