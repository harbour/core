
include $(TOP)$(ROOT)config/global.mk

ifneq ($(HB_PLATFORM),)
ifneq ($(HB_COMPILER),)

first::

ifeq ($(HB_INSTALL_DOC),no)
install::

else
ifeq ($(HB_INSTALL_DOC),)
install::

else
INSTALL_FILES := $(DOC_FILES)
INSTALL_DIR := $(HB_INSTALL_DOC)$(DOC_SUBDIR)
include $(TOP)$(ROOT)config/instsh.mk
INSTALL_RULE_DOC := $(INSTALL_RULE)
ifneq ($(INSTALL_RULE_DOC),)
install:: first
	$(INSTALL_RULE_DOC)

endif

endif
endif
endif
endif
