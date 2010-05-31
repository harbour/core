#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

vpath %.c ../

LIBNAME := hbssls

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

ifneq ($(HB_HAS_OPENSSL),)

   HB_CFLAGS += $(foreach d,$(HB_HAS_OPENSSL),-I$(d))

   HB_CFLAGS += -DHB_OPENSSL_STATIC

   # Patent will expire in 2010/2011
   HB_CFLAGS += -DOPENSSL_NO_IDEA

   include $(TOP)$(ROOT)config/lib.mk
else
   include $(TOP)$(ROOT)config/none.mk
endif
