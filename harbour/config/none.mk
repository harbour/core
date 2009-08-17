#
# $Id$
#

include $(TOP)$(ROOT)config/global.mk

ifneq ($(HB_ARCHITECTURE),)
ifneq ($(HB_COMPILER),)

ifneq ($(LIBNAME),)
   DIR_RULE := @echo $(ECHOQUOTE)! '$(LIBNAME)' library skipped$(ECHOQUOTE)
else
   DIR_RULE :=
endif

all : first

first clean install::
	$(DIR_RULE)

endif
endif
