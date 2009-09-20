#
# $Id$
#

include $(TOP)$(ROOT)config/global.mk

ifneq ($(HB_PLATFORM),)
ifneq ($(HB_COMPILER),)

HB_LINKING_RTL :=
HB_LINKING_VMMT :=

ifneq ($(DYNNAME),)
   HB_LINKING_RTL := yes
   ifneq ($(findstring vmmt, $(DYNDIRLIST)),)
      HB_LINKING_VMMT := yes
   endif
endif

-include $(TOP)$(ROOT)config/$(HB_PLATFORM)/libs.mk

# We're linking them directly to Harbour dynlib now.
#ifneq ($(HB_HAS_PCRE_LOCAL),)
#   SYSLIBS += hbpcre
#endif
#ifneq ($(HB_HAS_ZLIB_LOCAL),)
#   SYSLIBS += hbzlib
#endif

include $(TOP)$(ROOT)config/$(HB_PLATFORM)/$(HB_COMPILER).mk
include $(TOP)$(ROOT)config/c.mk
include $(TOP)$(ROOT)config/prg.mk

HB_LIBS_TPL = \
   hbextern \
   hbdebug \
   $(_HB_VM) \
   hbrtl \
   hblang \
   hbcpage \
   $(HB_GT_LIBS) \
   $(_HB_RDD) \
   hbrtl \
   $(_HB_VM) \
   hbmacro \
   hbcplr \
   hbpp \
   hbcommon

DYN_FILE :=
IMP_FILE :=

ifneq ($(HB_BUILD_DLL),no)
ifneq ($(DY_RULE),)

DYN_NAME := $(DYN_PREF)$(DYNNAME)$(DYN_EXT)
DYN_FILE := $(DYN_DIR)/$(DYN_NAME)
DYN_NAME2 := $(DYN_PREF)$(DYNNAME2)$(DYN_EXT)
DYN_FILE2 := $(DYN_DIR)/$(DYN_NAME2)

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
include $(TOP)$(ROOT)config/instsh.mk
INSTALL_RULE_DYN := $(INSTALL_RULE)
ifneq ($(INSTALL_RULE_DYN),)
install:: first
	$(INSTALL_RULE_DYN)

endif

endif
endif
endif
endif
