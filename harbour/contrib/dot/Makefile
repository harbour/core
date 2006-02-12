#
# $Id$
#

ifeq ($(HB_MAIN),)
HB_MAIN = std
endif

ROOT = ../../

PRG_SOURCES=\
	pp.prg \

PRG_MAIN=pp.prg

PRG_HEADERS = \
	rp_dot.src \
	rp_run.src \

LIBS=\
	debug \
	vm \
	rtl \
	lang \
	rdd \
	macro \
	pp \
	common \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)

include $(TOP)$(ROOT)config/bin.cf

install::
	$(INSTALL_RULE_HEADERS)
