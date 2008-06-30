#
# $Id$
#

ROOT = ../../

ifneq ($(HB_ARCHITECTURE),dos)

ifeq ($(HB_ARCHITECTURE),linux)
C_SOURCES=\
	tplinux.c \
	tpcommon.c \

endif
ifeq ($(HB_ARCHITECTURE),win32)
C_SOURCES=\
	tpwin32.c \
	tpcommon.c \

endif
ifeq ($(HB_ARCHITECTURE),os2)
C_SOURCES=\
	tpos2.c \
	tpcommon.c \

endif

PRG_SOURCES=\
	telepath.prg \

PRG_HEADERS=\
	telepath.ch \

LIBNAME=hbtpathy

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

endif
