#
# $Id$
#

ROOT := ../../

C_SOURCES := \
   hb_btree.c \

C_HEADERS := \
   hb_btree.h \

PRG_SOURCES := \
   tbtree.prg \

PRG_HEADERS := \
   hb_btree.ch \

LIBNAME := hbbtree

include $(TOP)$(ROOT)config/header.mk
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.mk

install::
	$(INSTALL_RULE_HEADERS)
