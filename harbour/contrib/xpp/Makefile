#
# $Id$
#

ROOT := ../../

LIBNAME := xpp

C_SOURCES := \
	binnumx.c \
	dbcmdx.c \
	dbdetacx.c \
	idlex.c \
	mousex.c \
	oemansix.c \
	philesx.c \

PRG_SOURCES := \
	browdbx.prg \
	dbfuncsx.prg \
	dbjoinx.prg \
	dblistx.prg \
	dbsortx.prg \
	dbstruxx.prg \
	dbtotalx.prg \
	dbupdatx.prg \
	tbcolumx.prg \
	tbrowsex.prg \
	tgetx.prg \
	thfuncx.prg \
	tthreadx.prg \
	typefilx.prg \

PRG_HEADERS := \
	xpp.ch \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
