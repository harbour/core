#
# $Id$
#

ROOT = ../../

C_SOURCES=\
	gdwrp.c \

PRG_HEADERS=\
	gd.ch \
	gdexternal.ch \

PRG_SOURCES=\
	gd.prg \
	gdbar.prg \
	gdchart.prg \
	gdimage.prg \
	gdbarcod.prg \

LIBNAME=hbgd

C_USR += -I/usr/include 

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
