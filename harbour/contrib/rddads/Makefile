#
# $Id$
#

ROOT = ../../

LIBNAME=rddads

ifneq ($(HB_ARCHITECTURE),dos)

ifeq ($(HB_INC_ADS),)
HB_INC_ADS = /usr/local/ads/acesdk $(HOME)/ads/acesdk
endif

HB_INC_ADS_OK += $(foreach d, $(HB_INC_ADS), $(if $(wildcard $(d)/ace.h),$(d),))

ifneq ($(strip $(HB_INC_ADS_OK)),)

HB_USER_CFLAGS += $(foreach d, $(HB_INC_ADS_OK), -I$(d))

C_SOURCES=\
	ads1.c \
	adsfunc.c \
	adsmgmnt.c \
	adsx.c \

PRG_HEADERS=\
	ads.ch \
	adsextrn.ch \

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
