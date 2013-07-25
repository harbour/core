ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

ifneq ($(DYNDIRLIST_BASE),)

   ifeq ($(__HB_BUILD_DYN_ST),yes)
      DYNDIRLIST := $(DYNDIRLIST_BASE) src/vm
   else
      DYNDIRLIST := $(DYNDIRLIST_BASE) src/vm/vmmt
   endif
   DYNNAME := $(HB_DYNLIB_BASE)

   include $(TOP)$(ROOT)config/dyn.mk

   ifeq ($(__HB_BUILD_DYN_2ND),yes)
      DIRS := 2nd
      include $(TOP)$(ROOT)config/dir.mk
   endif

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
