#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

ifneq ($(DYNDIRLIST_BASE),)

   DYNDIRLIST := $(DYNDIRLIST_BASE) source/vm/vmmt
   DYNNAME := harbourmt$(DYNNAME_POST)

   include $(TOP)$(ROOT)config/dyn.mk

   ifneq ($(INSTALL_RULE),)

      INSTALL_RULE_DYN := $(INSTALL_RULE)

      ifneq ($(HB_LIB_INSTALL),)
         INSTALL_FILES := $(IMP_FILE)
         INSTALL_DIR := $(HB_LIB_INSTALL)
         HB_INSTALL_DEF := yes
         include $(TOP)$(ROOT)config/install.mk
      endif

install::
	$(INSTALL_RULE_DYN)

   else
      include $(TOP)$(ROOT)config/none.mk
   endif
else
   $(warning ! Warning: Please run make from one level up)
   include $(TOP)$(ROOT)config/none.mk
endif
