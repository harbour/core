#
# $Id$
#

include $(TOP)$(ROOT)config/instsh.cf

ifneq ($(INSTALL_RULE),)
ifneq ($(HB_INSTALL_DEF),yes)
install:: first
	$(INSTALL_RULE)
endif
endif
