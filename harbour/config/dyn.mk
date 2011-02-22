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

include $(TOP)$(ROOT)config/$(HB_PLATFORM)/$(HB_COMPILER).mk
include $(TOP)$(ROOT)config/c.mk
include $(TOP)$(ROOT)config/prg.mk

ifeq ($(HB_LINKING_VMMT),yes)
   _HB_VM := hbvmmt
else
   _HB_VM := hbvm
endif

HB_DYN_LIBS := \
   hbcommon \
   hbpp \
   hbrtl \
   hbmacro \
   hblang \
   hbcpage \
   hbextern \
   hbrdd \
   rddntx \
   rddnsx \
   rddcdx \
   rddfpt \
   hbsix \
   hbhsx \
   hbusrrdd \
   gtcgi \
   gtpca \
   gtstd \
   gtwvt \
   gtgui \
   gtwin \
   gtos2 \
   gttrm \
   gtcrs \
   gtsln \
   gtxwc \
   $(_HB_VM) \
   hbmaindllh

ifneq ($(HB_HAS_PCRE_LOCAL),)
   HB_DYN_LIBS += hbpcre
endif
ifneq ($(HB_HAS_ZLIB_LOCAL),)
   HB_DYN_LIBS += hbzlib
endif

#   hbcplr \
#   hbdebug \

ifneq ($(HB_HAS_PCRE_LOCAL),)
   HB_DYN_LIBS += hbpcre
endif
ifneq ($(HB_HAS_ZLIB_LOCAL),)
   HB_DYN_LIBS += hbzlib
endif


DYN_FILE :=
IMP_FILE :=

ifneq ($(HB_BUILD_DYN),no)
ifneq ($(DY_RULE),)

DYN_NAME := $(DYN_PREF)$(DYNNAME)$(DYN_EXT)$(HB_DYNLIB_PEXT)
DYN_FILE := $(DYN_DIR)/$(DYN_NAME)
DYN_NAME2 := $(DYN_PREF)$(DYNNAME2)$(DYN_EXT)
DYN_FILE2 := $(DYN_DIR)/$(DYN_NAME2)

ifneq ($(IMP_DIR),)
   IMP_NAME := $(LIB_PREF)$(DYNNAME)$(LIB_EXT)
   IMP_FILE := $(IMP_DIR)/$(IMP_NAME)
endif

ifeq ($(HB_DYN_FROM_LIBS),yes)
   ALL_OBJS := $(subst /,$(DIRSEP),$(foreach lib,$(HB_DYN_LIBS),$(wildcard $(LIB_DIR)/$(LIB_PREF)$(lib)$(LIB_EXT))))
else
   ALL_OBJS := $(subst /,$(DIRSEP),$(foreach dir,$(DYNDIRLIST),$(wildcard $(TOP)$(ROOT)$(dir)/$(OBJ_DIR)/*$(OBJ_DYN_POSTFIX)$(OBJ_EXT))))
endif

first:: dirbase descend

descend:: dirbase
	+@$(MK) $(MKFLAGS) -C $(OBJ_DIR) -f $(GRANDP)Makefile TOP=$(GRANDP) $(DYN_NAME)

vpath $(DYN_NAME) $(DYN_DIR)
$(DYN_NAME) : $(ALL_OBJS)
	$(DY_RULE)

INSTALL_FILES := $(DYN_FILE)
INSTALL_DIR := $(HB_INSTALL_DYN)
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
