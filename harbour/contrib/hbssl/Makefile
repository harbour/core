#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbssl

ifeq ($(HB_HAS_OPENSSL),yes)

HB_CFLAGS += -I$(HB_INC_OPENSSL)
ifeq ($(HB_PLATFORM),darwin)
   HB_CFLAGS += -DOPENSSL_NO_SHA256 -DOPENSSL_NO_SHA512 -DOPENSSL_NO_CAMELLIA -DOPENSSL_NO_SEED -DOPENSSL_NO_DGRAM -DHB_OPENSSL_OLD_OSX_
endif

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

include $(TOP)$(ROOT)config/header.mk
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.mk

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.mk
endif
