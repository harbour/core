#
# $Id$
#

ifneq ($(HB_WITHOUT_ODBC),yes)
ifneq ($(HB_COMPILER),rsxnt)

ROOT = ../../

C_SOURCES= \
	odbc.c \

PRG_SOURCES= \
	todbc.prg \
	browodbc.prg \

PRG_HEADERS=\
	sql.ch \

LIBNAME=hbodbc

include $(TOP)$(ROOT)config/lib.cf

endif
endif
