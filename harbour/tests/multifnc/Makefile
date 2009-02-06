#
# $Id$
#

ifeq ($(HB_COMPILER),gcc)
HB_USER_LDFLAGS += -Wl,--allow-multiple-definition
else
ifeq ($(HB_COMPILER),gpp)
HB_USER_LDFLAGS += -Wl,--allow-multiple-definition
else
ifeq ($(HB_COMPILER),djgpp)
HB_USER_LDFLAGS += -Wl,--allow-multiple-definition
else
ifeq ($(HB_COMPILER),mingw)
HB_USER_LDFLAGS += -Wl,--allow-multiple-definition
else
ifeq ($(HB_COMPILER),xcc)
HB_USER_LDFLAGS += /FORCE:MULTIPLE
else
ifeq ($(HB_COMPILER),pocc)
HB_USER_LDFLAGS += /FORCE:MULTIPLE
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
