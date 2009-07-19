#
# $Id$
#

ROOT = ../../

LIBNAME=hbssl

HB_INC_OPENSSL_OK =

ifneq ($(HB_ARCHITECTURE),dos)
ifneq ($(HB_COMPILER),watcom)

ifeq ($(HB_INC_OPENSSL),)
ifeq ($(HB_XBUILD),)
HB_INC_OPENSSL = /usr/include /usr/local/ssl/include
endif
endif

HB_INC_OPENSSL_OK += $(foreach d, $(HB_INC_OPENSSL), $(if $(wildcard $(d)/openssl/ssl.h),$(d),))

endif

ifneq ($(strip $(HB_INC_OPENSSL_OK)),)

HB_USER_CFLAGS += $(foreach d, $(HB_INC_OPENSSL_OK), -I$(d))

C_SOURCES=\
	ssl.c \
	sslbio.c \
	sslciph.c \
	sslctx.c \
	sslerr.c \
	sslrand.c \
	sslsess.c \
	sslx509.c \

PRG_HEADERS=\
	hbssl.ch \

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
