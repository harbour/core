#
# $Id$
#

ifeq ($(HB_INC_ADS),)
HB_INC_ADS = /usr/local/ads/acesdk $(HOME)/ads/acesdk
endif

HB_INC_ADS_OK += $(foreach d, $(HB_INC_ADS), $(if $(wildcard $(d)/ace.h),$(d),))

ifneq ($(HB_INC_ADS_OK),)

C_USR += $(foreach d, $(HB_INC_ADS_OK), -I$(d))

ROOT = ../../

C_SOURCES=\
	ads1.c \
	adsfunc.c \
	adsmgmnt.c \

PRG_HEADERS=\
	ads.ch \

LIBNAME=rddads

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

endif
