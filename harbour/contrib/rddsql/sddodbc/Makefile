#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.cf

LIBNAME := sddodbc

ifeq ($(HB_COMPILER),rsxnt)
HB_WITHOUT_ODBC := yes
endif
ifeq ($(HB_ARCHITECTURE),dos)
HB_WITHOUT_ODBC := yes
endif
ifeq ($(HB_ARCHITECTURE),os2)
HB_WITHOUT_ODBC := yes
endif

ifneq ($(HB_ARCHITECTURE),win)
ifneq ($(HB_ARCHITECTURE),wce)
ifeq ($(HB_WITHOUT_ODBC),)

ifeq ($(HB_INC_ODBC),)
ifeq ($(HB_XBUILD),)
HB_INC_ODBC := /usr/include /usr/local/include
endif
endif

HB_INC_ODBC_OK += $(foreach d, $(HB_INC_ODBC), $(if $(wildcard $(d)/sql.h),$(d),))

ifneq ($(strip $(HB_INC_ODBC_OK)),)
HB_USER_CFLAGS += $(foreach d, $(HB_INC_ODBC_OK), -I$(d))
else
HB_WITHOUT_ODBC := yes
endif

endif
endif
endif

ifneq ($(HB_WITHOUT_ODBC),yes)

C_SOURCES := \
	odbcdd.c \

include $(TOP)$(ROOT)config/lib.cf

else
include $(TOP)$(ROOT)config/none.cf
endif
