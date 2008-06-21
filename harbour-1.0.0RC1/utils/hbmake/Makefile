#
# $Id$
#

ifeq ($(HB_MAIN),)
    HB_MAIN = std
endif

ROOT = ../../

PRG_SOURCES=\
	hbmake.prg \
	tmake.prg \
	hbmutils.prg \
	fclass1.prg \
	ffile1.prg \
	ft_funcs.prg \
	radios.prg \
	checks.prg \
	pickarry.prg \
	pickfile.prg \
	prb_stak.prg \

C_SOURCES=\
	hbmlang.c \
	readline.c \


PRG_MAIN=hbmake.prg

LIBS=\
	hbdebug \
	hbvm \
	hbrtl \
	hbrdd \
	hbrtl \
	hbvm \
	hbmacro \
	hbpp \
	hblang \
	hbcommon \

include $(TOP)$(ROOT)config/bin.cf

