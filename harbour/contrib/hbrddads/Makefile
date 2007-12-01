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

ifneq ($(wildcard /usr/local/ads/acesdk/ace.h),)
   C_USR += -I/usr/local/ads/acesdk
else
ifneq ($(wildcard $(HOME)/ads/acesdk/ace.h),)
   C_USR += -I/usr/local/ads/acesdk
endif
endif

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
