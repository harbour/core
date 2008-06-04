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
	hbdebug \
	hbvm \
	hbrtl \
	hblang \
	hbcpage \
	hbrdd \
	hbrtl \
	hbvm \
	hbmacro \
	hbpp \
	hbcplr \
	hbcommon \

ifeq ($(findstring -DHB_PCRE_REGEX, $(C_USR)),)
ifeq ($(findstring -DHB_POSIX_REGEX, $(C_USR)),)
LIBS += hbpcre
endif
endif

ifeq ($(findstring -DHB_EXT_ZLIB, $(C_USR)),)
LIBS += hbzlib
endif

include $(TOP)$(ROOT)config/bin.cf
