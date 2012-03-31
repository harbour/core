#
# $Id$
#

include $(TOP)$(ROOT)config/global.mk

ifneq ($(HB_PLATFORM),)
ifneq ($(HB_COMPILER),)

first::

ifeq ($(HB_INSTALL_LIB),)
install::

else
INSTALL_FILES := $(LIB_BINS)
INSTALL_DIR := $(HB_INSTALL_LIB)
include $(TOP)$(ROOT)config/instsh.mk
INSTALL_RULE_LIB_BINS := $(INSTALL_RULE)
ifneq ($(INSTALL_RULE_LIB_BINS),)
install:: first
	$(INSTALL_RULE_LIB_BINS)

endif

endif
endif
endif
