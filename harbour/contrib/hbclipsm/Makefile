#
# $Id$
#

ROOT := ../../

C_SOURCES := \
	date.c \
	environ.c \
	gauge.c \
	num.c \
	numceil.c \
	numfloor.c \
	stack.c \
	status.c \
	time.c \

PRG_SOURCES := \

PRG_HEADERS := \
	time87.ch \

LIBNAME := hbclipsm

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
