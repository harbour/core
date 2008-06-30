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

# a workaround of a problem with different include directories 
# for mysql header files between different unix distributions
C_USR += -I/usr/include/mysql -DBIG_JOINS=1

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
