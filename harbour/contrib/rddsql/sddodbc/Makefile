#
# $Id$
#

ROOT = ../../../

LIBNAME=sddodbc

ifeq ($(HB_INC_ODBC),)
ifeq ($(HB_XBUILD),)
HB_INC_ODBC = /usr/include /usr/local/include
endif
endif

HB_INC_ODBC_OK += $(foreach d, $(HB_INC_ODBC), $(if $(wildcard $(d)/sql.h),$(d),))

ifneq ($(strip $(HB_INC_ODBC_OK)),)

HB_USER_CFLAGS += $(foreach d, $(HB_INC_ODBC_OK), -I$(d))

C_SOURCES=\
	odbcdd.c \

include $(TOP)$(ROOT)config/lib.cf
else
include $(TOP)$(ROOT)config/none.cf
endif
