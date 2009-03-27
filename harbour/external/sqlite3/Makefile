#
# $Id$
#

HB_BUILD_WARN = no
HB_BUILD_MODE = c

ROOT = ../../

LIBNAME=sqlite3

ifeq ($(HB_WITH_SQLITE3),)
ifeq ($(HB_ARCHITECTURE),win)
HB_WITH_SQLITE3=yes
endif
ifeq ($(HB_ARCHITECTURE),os2)
HB_WITH_SQLITE3=yes
endif
endif

ifeq ($(HB_WITH_SQLITE3),yes)
C_SOURCES =\
	sqlite3.c \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
