#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := rddads

ifneq ($(HB_PLATFORM),dos)
ifneq ($(HB_PLATFORM),os2)

ifeq ($(HB_INC_ADS),)
   HB_INC_ADS := /usr/local/ads/acesdk $(HOME)/ads/acesdk
endif

HB_INC_ADS_OK := $(foreach d, $(HB_INC_ADS), $(if $(wildcard $(d)/ace.h),$(d),))

endif
endif

ifneq ($(strip $(HB_INC_ADS_OK)),)

HB_CFLAGS += $(foreach d,$(HB_INC_ADS_OK),-I$(d))

C_SOURCES := \
   ads1.c \
   adsfunc.c \
   adsmgmnt.c \
   adsx.c \

PRG_HEADERS := \
   ads.ch \
   adsextrn.ch \

include $(TOP)$(ROOT)config/header.mk
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.mk

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.mk
endif
