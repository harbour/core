include $(TOP)$(ROOT)config/global.mk

ifneq ($(HB_PLATFORM),)
ifneq ($(HB_COMPILER),)

first::

ifeq ($(HB_INSTALL_INC),)
install::

else
INSTALL_FILES := $(C_HEADERS) $(PRG_HEADERS)
INSTALL_DIR := $(HB_INSTALL_INC)
include $(TOP)$(ROOT)config/instsh.mk
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
ifneq ($(INSTALL_RULE_HEADERS),)
install:: first
	$(INSTALL_RULE_HEADERS)

endif

endif
endif
endif
