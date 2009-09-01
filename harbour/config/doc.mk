#
# $Id$
#

include $(TOP)$(ROOT)config/global.mk

ifneq ($(HB_PLATFORM),)
ifneq ($(HB_COMPILER),)

first::

ifeq ($(HB_DOC_INSTALL),)
install::

else
INSTALL_FILES := $(DOC_FILES)
INSTALL_DIR := $(HB_DOC_INSTALL)$(DOC_SUBDIR)
include $(TOP)$(ROOT)config/instsh.mk
INSTALL_RULE_DOC := $(INSTALL_RULE)
ifneq ($(INSTALL_RULE_DOC),)
install:: first
	$(INSTALL_RULE_DOC)

endif

endif
endif
endif
