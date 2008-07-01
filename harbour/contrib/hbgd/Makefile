#
# $Id$
#

ifeq ($(HB_INC_GD),)
HB_INC_GD = /usr/include /opt/local/include
endif

HB_INC_GD_OK += $(foreach d, $(HB_INC_GD), $(if $(wildcard $(d)/gd.h),$(d),))

ifneq ($(strip $(HB_INC_GD_OK)),)

C_USR += $(foreach d, $(HB_INC_GD_OK), -I$(d))

ROOT = ../../

C_SOURCES=\
	gdwrp.c \

PRG_HEADERS=\
	gd.ch \
	gdexternal.ch \

PRG_SOURCES=\
	gd.prg \
	gdbar.prg \
	gdchart.prg \
	gdimage.prg \
	gdbarcod.prg \

LIBNAME=hbgd

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

endif
