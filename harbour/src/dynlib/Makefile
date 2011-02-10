#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

ifneq ($(DYNDIRLIST_BASE),)

   ifeq ($(_HB_BUILD_DYN_ST),yes)
      DYNDIRLIST := $(DYNDIRLIST_BASE) src/vm
   else
      DYNDIRLIST := $(DYNDIRLIST_BASE) src/vm/vmmt
   endif
   DYNNAME := $(HB_DYNLIB_ST)
   DYNNAME2 := $(HB_DYNLIB_BASE_ST)

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
