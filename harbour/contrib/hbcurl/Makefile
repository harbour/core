#
# $Id$
#

ROOT = ../../

LIBNAME=hbcurl

ifneq ($(HB_ARCHITECTURE),dos)

ifeq ($(HB_INC_CURL),)
HB_INC_CURL = /usr/include
endif

HB_INC_CURL_OK += $(foreach d, $(HB_INC_CURL), $(if $(wildcard $(d)/curl/curl.h),$(d),))

ifneq ($(strip $(HB_INC_CURL_OK)),)

C_USR += $(foreach d, $(HB_INC_CURL_OK), -I$(d))

C_SOURCES=\
	hbcurl.c \

PRG_HEADERS=\
	hbcurl.ch \

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
