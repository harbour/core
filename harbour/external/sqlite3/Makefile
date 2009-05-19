#
# $Id$
#

HB_BUILD_WARN = no
HB_BUILD_MODE = c

ifeq ($(HB_ARCHITECTURE),wce)
HB_USER_CFLAGS += -D_WIN32_WCE
endif

ROOT = ../../

LIBNAME=sqlite3

ifeq ($(HB_WITH_SQLITE3),)
ifeq ($(HB_ARCHITECTURE),win)
HB_WITH_SQLITE3=yes
endif
ifeq ($(HB_ARCHITECTURE),wce)
HB_WITH_SQLITE3=yes
endif
ifeq ($(HB_ARCHITECTURE),os2)
HB_WITH_SQLITE3=yes
endif
ifeq ($(HB_COMPILER),poccarm)
HB_WITH_SQLITE3=no
endif
endif

ifeq ($(HB_WITH_SQLITE3),yes)
C_SOURCES =\
	sqlite3.c \

include $(TOP)$(ROOT)config/lib.cf

else
include $(TOP)$(ROOT)config/none.cf
endif
