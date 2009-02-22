#
# $Id$
#

ifeq ($(HB_MAIN),)
HB_MAIN = std
endif

ROOT = ../../

PRG_SOURCES=\
	hbdoc.prg \
	genos2.prg \
	gentrf.prg \
	genng.prg \
	genhtm.prg \
	genchm.prg \
	genrtf.prg \
	genhpc.prg \
	genasc.prg \
	genpdf1.prg \
	ft_funcs.prg \

C_SOURCES=\
	hbdfrdln.c \

PRG_MAIN=hbdoc.prg

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
