#
# $Id$
#

ROOT := ../

include $(ROOT)config/global.mk

all : first

first clean install::
	@$(TRUE)

clean::
	$(if $(wildcard $(HB_HOST_BIN_DIR)/hbrun$(HB_HOST_BIN_EXT)),+$(HB_HOST_BIN_DIR)/hbrun$(HB_HOST_BIN_EXT) --hb:gtcgi $(TOP)$(ROOT)contrib/make.hbs clean,@$(ECHO) $(ECHOQUOTE)! Warning: hbrun not found, contrib/make.hbs skipped.$(ECHOQUOTE))

install::
	$(if $(wildcard $(HB_HOST_BIN_DIR)/hbrun$(HB_HOST_BIN_EXT)),+$(HB_HOST_BIN_DIR)/hbrun$(HB_HOST_BIN_EXT) --hb:gtcgi $(TOP)$(ROOT)contrib/make.hbs install,@$(ECHO) $(ECHOQUOTE)! Warning: hbrun not found, contrib/make.hbs skipped.$(ECHOQUOTE))

all:
	$(if $(wildcard $(HB_HOST_BIN_DIR)/hbrun$(HB_HOST_BIN_EXT)),+$(HB_HOST_BIN_DIR)/hbrun$(HB_HOST_BIN_EXT) --hb:gtcgi $(TOP)$(ROOT)contrib/make.hbs all,@$(ECHO) $(ECHOQUOTE)! Warning: hbrun not found, contrib/make.hbs skipped.$(ECHOQUOTE))
