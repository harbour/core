#
# $Id$
#

ROOT = ../../

LIBNAME=hbsqlit3

ifneq ($(HB_COMPILER),pocc)
ifneq ($(HB_COMPILER),pocc64)
ifneq ($(HB_COMPILER),poccarm)

ifeq ($(HB_INC_SQLITE3),)
ifeq ($(HB_XBUILD),)
HB_INC_SQLITE3 = /usr/include
ifeq ($(HB_ARCHITECTURE),win)
HB_INC_SQLITE3 += $(TOP)$(ROOT)external/sqlite3
endif
ifeq ($(HB_ARCHITECTURE),wce)
HB_INC_SQLITE3 += $(TOP)$(ROOT)external/sqlite3
endif
ifeq ($(HB_ARCHITECTURE),os2)
HB_INC_SQLITE3 += $(TOP)$(ROOT)external/sqlite3
endif
endif
endif

HB_INC_SQLITE3_OK += $(foreach d, $(HB_INC_SQLITE3), $(if $(wildcard $(d)/sqlite3.h),$(d),))

ifneq ($(strip $(HB_INC_SQLITE3_OK)),)

HB_USER_CFLAGS += $(foreach d, $(HB_INC_SQLITE3_OK), -I$(d))

C_SOURCES=\
	hbsqlit3.c \

PRG_HEADERS=\
	hbsqlit3.ch \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
else
include $(TOP)$(ROOT)config/none.cf
endif
else
include $(TOP)$(ROOT)config/none.cf
endif
else
include $(TOP)$(ROOT)config/none.cf
endif
else
include $(TOP)$(ROOT)config/none.cf
endif
