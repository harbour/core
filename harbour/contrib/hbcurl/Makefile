#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.cf

LIBNAME := hbcurl

HB_INC_CURL_OK :=

ifneq ($(HB_ARCHITECTURE),dos)

ifeq ($(HB_INC_CURL),)
   ifeq ($(HB_XBUILD),)
      HB_INC_CURL := /usr/include
   endif
endif

HB_INC_CURL_OK := $(foreach d, $(HB_INC_CURL), $(if $(wildcard $(d)/curl/curl.h),$(d),))

endif

ifneq ($(strip $(HB_INC_CURL_OK)),)

HB_CFLAGS += $(foreach d,$(HB_INC_CURL_OK),-I$(d))

C_SOURCES := \
   hbcurl.c \
   hbcurlm.c \

PRG_HEADERS := \
   hbcurl.ch \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

DIRS :=
ifeq ($(HB_ARCHITECTURE),win)
   DIRS += hbcurls
endif
ifeq ($(HB_ARCHITECTURE),wce)
   DIRS += hbcurls
endif

ifneq ($(DIRS),)
   include $(TOP)$(ROOT)config/dir.cf
endif

else
include $(TOP)$(ROOT)config/none.cf
endif
