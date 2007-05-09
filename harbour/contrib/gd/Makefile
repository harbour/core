#
# $Id$
#

ROOT = ../../

C_SOURCES=\
	gdwrp.c \

PRG_HEADERS=\
	gd.ch \

PRG_SOURCES=\
	gd.prg  \
	gdbar.prg \
	gdchart.prg \
	gdimage.prg \
	gdbarcod.prg \

LIBNAME=hbgd

ifeq ($(HB_COMPILER),mingw32)
   CFLAGS := $(CFLAGS) -I/usr/include 
else
   CFLAGS := $(CFLAGS) -I/usr/include
endif

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

