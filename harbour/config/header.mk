#
# $Id$
#

include $(TOP)$(ROOT)config/global.cf

ifneq ($(HB_ARCHITECTURE),)
ifneq ($(HB_COMPILER),)

first::

ifeq ($(HB_INC_INSTALL),)
install::

else
INSTALL_FILES := $(C_HEADERS) $(PRG_HEADERS) $(API_HEADERS)
INSTALL_DIR := $(HB_INC_INSTALL)

include $(TOP)$(ROOT)config/install.cf
endif

endif
endif
