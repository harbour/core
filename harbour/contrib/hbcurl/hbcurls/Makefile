#
# $Id$
#

vpath %.c ../

ROOT = ../../../

LIBNAME=hbcurls

# always export symbols
HB_USER_CFLAGS += -DCURL_STATICLIB

HB_INC_CURL_OK =

ifneq ($(HB_ARCHITECTURE),dos)

ifeq ($(HB_INC_CURL),)
ifeq ($(HB_XBUILD),)
HB_INC_CURL = /usr/include
endif
endif

HB_INC_CURL_OK += $(foreach d, $(HB_INC_CURL), $(if $(wildcard $(d)/curl/curl.h),$(d),))

endif

ifneq ($(strip $(HB_INC_CURL_OK)),)

HB_USER_CFLAGS += $(foreach d, $(HB_INC_CURL_OK), -I$(d))

C_SOURCES=\
	hbcurl.c \
	hbcurlm.c \

include $(TOP)$(ROOT)config/lib.cf

else
include $(TOP)$(ROOT)config/none.cf
endif
