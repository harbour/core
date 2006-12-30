#
# $Id$
#

ifeq ($(HB_COMPILER),xcc)
C_USR:=$(subst -Ox,,$(subst -Ot,,$(C_USR)))
endif

ROOT = ../../../

C_SOURCES=\
	dbffpt1.c \

LIBNAME=dbffpt

include $(TOP)$(ROOT)config/lib.cf
