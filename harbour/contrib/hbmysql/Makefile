#
# $Id$
#

ROOT = ../../

C_SOURCES=\
	mysql.c \

PRG_SOURCES=\
	tmysql.prg \
	tsqlbrw.prg \

PRG_HEADERS=\
	mysql.ch \

LIBNAME=hbmysql

include $(TOP)$(ROOT)config/lib.cf

# a workaround of a problem with different include directories 
# for mysql header files between different unix distributions
CFLAGS := -I/usr/include/mysql -DBIG_JOINS=1
