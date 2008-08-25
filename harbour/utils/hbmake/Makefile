#
# $Id$
#

ifeq ($(HB_MAIN),)
    HB_MAIN = std
endif

ROOT = ../../

PRG_SOURCES=\
	hbmake.prg \
	hbmutils.prg \

C_SOURCES=\
	hbmfrdln.c \
	hbmgauge.c \
	hbmlang.c \

PRG_MAIN=hbmake.prg

LIBS=\
	hbdebug \
	hbvm \
	hbrtl \
	hbnulrdd \
	hbrtl \
	hbvm \
	hbmacro \
	hbcommon \

include $(TOP)$(ROOT)config/bin.cf
