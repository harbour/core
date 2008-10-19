#
# $Id: Makefile 7358 2007-05-09 08:08:49Z lf_sfnet $
#

ROOT = ../../

C_SOURCES=\
	dbgfxc.c \

PRG_SOURCES=\
	dbgfx.prg  \
	sprinf.prg \

LIBNAME=hbdbgfx

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

