#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

ifneq ($(DYNDIRLIST_BASE),)
   ifneq ($(HB_ARCHITECTURE),dos)

      DYNDIRLIST := $(DYNDIRLIST_BASE) source/vm/vmmt
      DYNNAME := harbourmt$(DYNNAME_POST)

      include $(TOP)$(ROOT)config/dyn.mk
   else
      include $(TOP)$(ROOT)config/none.mk
   endif
else
   $(warning ! Warning: Please run make from one level up)
   include $(TOP)$(ROOT)config/none.mk
endif
