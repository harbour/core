#
# $Id$
#

include $(TOP)$(ROOT)config/global.mk

ifneq ($(HB_PLATFORM),)
ifneq ($(HB_COMPILER),)

first::

ifeq ($(HB_INC_INSTALL),)
install::

else
INSTALL_FILES := $(C_HEADERS) $(PRG_HEADERS) $(API_HEADERS)
INSTALL_DIR := $(HB_INC_INSTALL)

include $(TOP)$(ROOT)config/install.mk
endif

endif
endif
