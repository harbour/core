#
# $Id$
#

ROOT = ../../

LIBNAME=hbfimage

ifeq ($(HB_INC_FREEIMAGE),)
HB_INC_FREEIMAGE = /usr/include
endif

HB_INC_FREEIMAGE_OK += $(foreach d, $(HB_INC_FREEIMAGE), $(if $(wildcard $(d)/FreeImage.h),$(d),))

ifneq ($(strip $(HB_INC_FREEIMAGE_OK)),)

C_USR += $(foreach d, $(HB_INC_FREEIMAGE_OK), -I$(d))

C_SOURCES = \
	fi_winfu.c \
	fi_wrp.c \

PRG_SOURCES= \

PRG_HEADERS=\
	freeimage.ch \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
