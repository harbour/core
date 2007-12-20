#
# $Id$
#

ROOT = ../../

C_SOURCES=\
   ads1.c \
   adsfunc.c \
   adsmgmnt.c \

PRG_HEADERS=\
	ads.ch \

LIBNAME=hbrddads

ifeq ($(HB_ARCHITECTURE),w32)
   C_USR += -DWIN32
endif

ifeq ($(ACE_DIRS),)
export ACE_DIRS = /usr/local $(HOME)
C_USR += $(foreach d, $(ACE_DIRS), $(if $(wildcard $(d)/ads/acesdk/ace.h),-I$(d)/ads/acesdk,))
endif

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
