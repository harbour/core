#
# $Id$
#

ROOT := ../

include $(ROOT)config/global.mk

ifneq ($(HB_NO_HBSCRIPT),yes)

first clean install::
	$(if $(wildcard $(HB_HOST_BIN_DIR)/hbrun$(HB_HOST_BIN_EXT)),-$(HB_HOST_BIN_DIR)/hbrun$(HB_HOST_BIN_EXT) --hb:gtcgi $(TOP)$(ROOT)contrib/make.hbs $@,@$(ECHO) $(ECHOQUOTE)! Warning: hbrun not found, contrib/make.hbs skipped.$(ECHOQUOTE))

endif
