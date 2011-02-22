#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

ifneq ($(DYNDIRLIST_BASE),)

   DYNDIRLIST := $(DYNDIRLIST_BASE) src/vm
   DYNNAME := $(HB_DYNLIB_BASE_2ND)

   include $(TOP)$(ROOT)config/dyn.mk

   ifneq ($(INSTALL_RULE_DYN),)
      ifneq ($(HB_INSTALL_LIB),)
         ifneq ($(IMP_FILE),)
            INSTALL_FILES := $(IMP_FILE)
            INSTALL_DIR := $(HB_INSTALL_LIB)
            include $(TOP)$(ROOT)config/install.mk
         endif
      endif
   else
      include $(TOP)$(ROOT)config/none.mk
   endif
else
   $(warning ! Warning: Please run make from one level up)
   include $(TOP)$(ROOT)config/none.mk
endif
