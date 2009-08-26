#
# $Id$
#

include $(TOP)$(ROOT)config/global.mk

ifneq ($(HB_PLATFORM),)
ifneq ($(HB_COMPILER),)

-include $(TOP)$(ROOT)config/$(HB_PLATFORM)/libs.mk
include $(TOP)$(ROOT)config/$(HB_PLATFORM)/$(HB_COMPILER).mk
include $(TOP)$(ROOT)config/c.mk
include $(TOP)$(ROOT)config/prg.mk

DYN_FILE :=
IMP_FILE :=

ifneq ($(HB_BUILD_DLL),no)
ifneq ($(DY_RULE),)

DYN_NAME := $(DYN_PREF)$(DYNNAME)$(DYN_EXT)
DYN_FILE := $(DYN_DIR)/$(DYN_NAME)

ifneq ($(IMP_DIR),)
   IMP_NAME := $(LIB_PREF)$(DYNNAME)$(LIB_EXT)
   IMP_FILE := $(IMP_DIR)/$(IMP_NAME)
endif

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
endif
