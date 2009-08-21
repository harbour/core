#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

ifneq ($(DYNDIRLIST_BASE),)
   ifneq ($(HB_ARCHITECTURE),dos)

      DYNDIRLIST := $(DYNDIRLIST_BASE) source/vm
      DYNNAME := harbour$(DYNNAME_POST)

      DIRS := mt

      include $(TOP)$(ROOT)config/dyn.mk
      include $(TOP)$(ROOT)config/dir.mk
   else
      include $(TOP)$(ROOT)config/none.mk
   endif
else
   $(warning ! Warning: Please run make from one level up)
   include $(TOP)$(ROOT)config/none.mk
endif
