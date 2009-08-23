#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbodbc

ifeq ($(HB_COMPILER),rsxnt)
   HB_WITHOUT_ODBC := yes
endif
ifeq ($(HB_PLATFORM),dos)
   HB_WITHOUT_ODBC := yes
endif
ifeq ($(HB_PLATFORM),os2)
   HB_WITHOUT_ODBC := yes
endif

ifneq ($(HB_PLATFORM),win)
ifneq ($(HB_PLATFORM),wce)
ifeq ($(HB_WITHOUT_ODBC),)

ifeq ($(HB_INC_ODBC),)
   ifeq ($(HB_XBUILD),)
      HB_INC_ODBC := /usr/include /usr/local/include
   endif
endif

HB_INC_ODBC_OK := $(foreach d, $(HB_INC_ODBC), $(if $(wildcard $(d)/sql.h),$(d),))

ifneq ($(strip $(HB_INC_ODBC_OK)),)
   HB_CFLAGS += $(foreach d,$(HB_INC_ODBC_OK),-I$(d))
else
   HB_WITHOUT_ODBC := yes
endif

endif
endif
endif

ifneq ($(HB_WITHOUT_ODBC),yes)

C_SOURCES := \
   odbc.c \
   odbcold.c \

PRG_SOURCES := \
   todbc.prg \
   browodbc.prg \

PRG_HEADERS := \
   sql.ch \

include $(TOP)$(ROOT)config/header.mk
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.mk

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.mk
endif
