#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbssl

C_SOURCES := \
   hbssl.c \
   bio.c \
   err.c \
   evp.c \
   evpciph.c \
   evpenc.c \
   evpmd.c \
   evppkey.c \
   pem.c \
   rand.c \
   ssl.c \
   sslciph.c \
   sslctx.c \
   sslsess.c \
   x509.c \

PRG_HEADERS := \
   hbssl.ch \

_DET_DSP_NAME := openssl
_DET_VAR_INC_ := HB_INC_OPENSSL
_DET_VAR_HAS_ := HB_HAS_OPENSSL
_DET_FLT_PLAT := !wce
_DET_FLT_COMP :=
_DET_INC_DEFP := /usr/include /usr/local/include /usr/local/ssl/include /boot/common/include
_DET_INC_HEAD := /openssl/ssl.h

include $(TOP)$(ROOT)config/detfun.mk

ifneq ($(HB_HAS_OPENSSL),)

   HB_CFLAGS += $(foreach d,$(HB_HAS_OPENSSL),-I$(d))
   ifeq ($(HB_PLATFORM),darwin)
      HB_CFLAGS += -DOPENSSL_NO_SHA256 -DOPENSSL_NO_SHA512 -DOPENSSL_NO_CAMELLIA -DOPENSSL_NO_SEED -DOPENSSL_NO_DGRAM -DHB_OPENSSL_OLD_OSX_
   endif
   # Patent will expire in 2010/2011
   HB_CFLAGS += -DOPENSSL_NO_IDEA

   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk

   ifneq ($(filter $(HB_PLATFORM),win wce),)
      DIRS += hbssls
      include $(TOP)$(ROOT)config/dir.mk
   endif
else
   HB_SKIP_REASON := component not found
   include $(TOP)$(ROOT)config/none.mk
endif
