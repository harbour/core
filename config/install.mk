
include $(TOP)$(ROOT)config/instsh.mk

ifneq ($(INSTALL_RULE),)
install:: first
	$(INSTALL_RULE)
endif
