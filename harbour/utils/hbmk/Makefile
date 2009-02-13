#
# $Id$
#

ifeq ($(HB_MAIN),)
HB_MAIN = std
endif

ROOT = ../../

PRG_SOURCES=\
	hbmk.prg \

PRG_MAIN=hbmk.prg

LIBS=\
	hbdebug \
	hbvm \
	hbrtl \
	hblang \
	hbcpage \
	hbnulrdd \
	hbrtl \
	hbvm \
	hbmacro \
	hbcommon \


HB_USER_PRGFLAGS += -gc0

include $(TOP)$(ROOT)config/bin.cf
