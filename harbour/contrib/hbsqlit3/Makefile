#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbsqlit3

ifeq ($(HB_INC_SQLITE3),)
   ifeq ($(HB_XBUILD),)
      HB_INC_SQLITE3 := /usr/include
      ifeq ($(HB_PLATFORM),win)
         HB_INC_SQLITE3 += $(TOP)$(ROOT)external/sqlite3
      endif
      ifeq ($(HB_PLATFORM),wce)
         HB_INC_SQLITE3 += $(TOP)$(ROOT)external/sqlite3
      endif
      ifeq ($(HB_PLATFORM),os2)
         HB_INC_SQLITE3 += $(TOP)$(ROOT)external/sqlite3
      endif
      ifeq ($(HB_PLATFORM),dos)
         HB_INC_SQLITE3 += $(TOP)$(ROOT)external/sqlite3
      endif
   endif
endif

HB_INC_SQLITE3_OK := $(foreach d, $(HB_INC_SQLITE3), $(if $(wildcard $(d)/sqlite3.h),$(d),))

ifneq ($(strip $(HB_INC_SQLITE3_OK)),)

HB_CFLAGS += $(foreach d,$(HB_INC_SQLITE3_OK),-I$(d))

C_SOURCES := \
   hbsqlit3.c \

PRG_HEADERS := \
   hbsqlit3.ch \

include $(TOP)$(ROOT)config/header.mk
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.mk

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.mk
endif
