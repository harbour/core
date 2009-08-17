#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

vpath %.c ../

LIBNAME := hbcurls

HB_INC_CURL_OK := $(foreach d, $(HB_INC_CURL), $(if $(wildcard $(d)/curl/curl.h),$(d),))

ifneq ($(strip $(HB_INC_CURL_OK)),)

# always export symbols
HB_CFLAGS += -DCURL_STATICLIB
HB_CFLAGS += $(foreach d,$(HB_INC_CURL_OK),-I$(d))

C_SOURCES := \
   hbcurl.c \
   hbcurlm.c \

include $(TOP)$(ROOT)config/lib.mk

else
include $(TOP)$(ROOT)config/none.mk
endif
