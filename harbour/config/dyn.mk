#
# $Id$
#

include $(TOP)$(ROOT)config/global.mk

ifneq ($(HB_BUILD_DLL),no)
ifneq ($(HB_ARCHITECTURE),)
ifneq ($(HB_COMPILER),)

include $(TOP)$(ROOT)config/$(HB_ARCHITECTURE)/$(HB_COMPILER).mk
include $(TOP)$(ROOT)config/c.mk
include $(TOP)$(ROOT)config/prg.mk

DYN_NAME := $(DYN_PREF)$(DYNNAME)$(DYN_EXT)

DYN_FILE := $(DYN_DIR)/$(DYN_NAME)

ALL_OBJS := $(subst /,$(DIRSEP),$(foreach dir,$(DYNDIRLIST),$(wildcard $(TOP)$(ROOT)$(dir)/$(OBJ_DIR)/*$(OBJ_DYN_POSTFIX)$(OBJ_EXT))))

first:: dirbase descend

descend:: dirbase
	+@$(MK) $(MKFLAGS) -C $(OBJ_DIR) -f $(GRANDP)Makefile TOP=$(GRANDP) $(DYN_NAME)

vpath $(DYN_NAME) $(DYN_DIR)
$(DYN_NAME) : $(ALL_OBJS)
	$(DY_RULE)

INSTALL_FILES := $(DYN_FILE)
INSTALL_DIR := $(HB_DYN_INSTALL)

include $(TOP)$(ROOT)config/install.mk

endif
endif
endif
