#
# $Id$
#

ifeq ($(HB_MAIN),)
HB_MAIN = std
endif

ROOT = ../../

PRG_SOURCES=\
	hbrun.prg \
	external.prg \

PRG_MAIN=hbrun.prg

LIBS=\
	debug \
	vm \
	rtl \
	lang \
	rdd \
	rtl \
	vm \
	macro \
	pp \
	compiler \
	common \

ifeq ($(findstring -DHB_PCRE_REGEX, $(C_USR)),)
ifeq ($(findstring -DHB_POSIX_REGEX, $(C_USR)),)
LIBS += hbpcre
endif
endif

include $(TOP)$(ROOT)config/bin.cf
