#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.cf

LIBNAME := hbfimage

HB_INC_FREEIMAGE_OK :=

ifneq ($(HB_ARCHITECTURE),dos)
# disabled until a proper solution is found for type collision
ifneq ($(HB_ARCHITECTURE),os2)
ifneq ($(HB_ARCHITECTURE),linux)
ifneq ($(HB_ARCHITECTURE),darwin)

ifeq ($(HB_INC_FREEIMAGE),)
   ifeq ($(HB_XBUILD),)
      HB_INC_FREEIMAGE := /usr/include /opt/local/include
   endif
endif

HB_INC_FREEIMAGE_OK := $(foreach d, $(HB_INC_FREEIMAGE), $(if $(wildcard $(d)/FreeImage.h),$(d),))
endif

ifneq ($(strip $(HB_INC_FREEIMAGE_OK)),)

HB_USER_CFLAGS += $(foreach d, $(HB_INC_FREEIMAGE_OK), -I$(d))

C_SOURCES := \
   fi_winfu.c \
   fi_wrp.c \

PRG_SOURCES := \

PRG_HEADERS := \
   freeimage.ch \

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
else
include $(TOP)$(ROOT)config/none.cf
endif
else
include $(TOP)$(ROOT)config/none.cf
endif
