#
# $Id$
#

# You can get the library from this link:
#    http://www.libgd.org/Downloads

# On Linux you'll need these packages:
#    gd, gd-devel

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbgd

ifeq ($(HB_INC_GD),)
   ifeq ($(HB_XBUILD),)
      HB_INC_GD := /usr/include /opt/local/include
   endif
endif

HB_INC_GD_OK := $(foreach d, $(HB_INC_GD), $(if $(wildcard $(d)/gd.h),$(d),))

ifneq ($(strip $(HB_INC_GD_OK)),)

HB_CFLAGS += $(foreach d,$(HB_INC_GD_OK),-I$(d))

C_SOURCES := \
   gdwrp.c \

PRG_HEADERS := \
   gd.ch \
   gdextern.ch \

PRG_SOURCES := \
   gd.prg \
   gdbar.prg \
   gdchart.prg \
   gdimage.prg \
   gdbarcod.prg \

include $(TOP)$(ROOT)config/header.mk
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.mk

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.mk
endif
