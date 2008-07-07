#
# $Id$
#

ROOT = ../../

LIBNAME=hbtpathy

C_SOURCES=

ifeq ($(HB_ARCHITECTURE),linux)
C_SOURCES=\
	tplinux.c \
	tpcommon.c \

endif
ifeq ($(HB_ARCHITECTURE),w32)
C_SOURCES=\
	tpwin32.c \
	tpcommon.c \

endif
ifeq ($(HB_ARCHITECTURE),os2)
C_SOURCES=\
	tpos2.c \
	tpcommon.c \

endif

ifneq ($(C_SOURCES),)

PRG_SOURCES=\
	telepath.prg \

PRG_HEADERS=\
	telepath.ch \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
