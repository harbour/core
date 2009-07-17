#
# $Id$
#

ifeq ($(HB_MAIN),)
HB_MAIN = std
endif

ROOT = ../../

PRG_SOURCES=\
	hbrun.prg \

PRG_MAIN=hbrun.prg

LIBS=\
	hbextern \
	hbdebug \
	hbvm \
	hbrtl \
	hblang \
	hbcpage \
	hbrdd \
	hbrtl \
	hbvm \
	hbmacro \
	hbcplr \
	hbpp \
	hbcommon \

ifeq ($(findstring -DHB_PCRE_REGEX, $(HB_USER_CFLAGS)),)
ifeq ($(findstring -DHB_POSIX_REGEX, $(HB_USER_CFLAGS)),)
LIBS += hbpcre
endif
endif

ifeq ($(findstring -DHB_EXT_ZLIB, $(HB_USER_CFLAGS)),)
LIBS += hbzlib
endif

include $(TOP)$(ROOT)config/bin.cf
