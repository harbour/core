#
# $Id$
#

ifeq ($(HB_COMPILER),gcc)
L_USR += -Wl,--allow-multiple-definition
else
ifeq ($(HB_COMPILER),gpp)
L_USR += -Wl,--allow-multiple-definition
else
ifeq ($(HB_COMPILER),djgpp)
L_USR += -Wl,--allow-multiple-definition
else
ifeq ($(HB_COMPILER),mingw32)
L_USR += -Wl,--allow-multiple-definition
else
ifeq ($(HB_COMPILER),xcc)
L_USR += /FORCE:MULTIPLE
else
ifeq ($(HB_COMPILER),pocc)
L_USR += /FORCE:MULTIPLE
endif
endif
endif
endif
endif
endif

ifeq ($(HB_MAIN),)
HB_MAIN = std
endif

ROOT = ../../

PRG_SOURCES=\
	t0.prg \
	t1.prg \
	t2.prg \

PRG_MAIN=t0.prg

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
	hbcommon \

include $(TOP)$(ROOT)config/bin.cf
