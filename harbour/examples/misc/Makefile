#
# $Id$
#

ROOT = ../../

LIBS=\
	hbdebug \
	hbvm \
	hbrtl \
	hblang \
	hbrdd \
	hbrtl \
	hbvm \
	hbmacro \
	hbpp \
	rddntx \
	rddcdx \
	rddfpt \
	hbsix \
	hbcommon \

ifeq ($(PM),)
  PM := $(pm)
endif
ifeq ($(PM),) # PM not defined = build all files
PRG_SOURCES=\
	guess.prg \
	mankala.prg \

PRG_HEADERS=\

C_SOURCES=\

C_HEADERS=\

include $(TOP)$(ROOT)config/test.cf

else #PM defined = build specified file

ifneq ($(findstring .prg,$(PM)),)
  PRG_MAIN := $(PM)
else
  ifneq ($(findstring .PRG,$(PM)),)
    PRG_MAIN := $(PM)
  else
    PRG_MAIN := $(PM).prg
  endif
endif
include $(TOP)$(ROOT)config/bin.cf

endif
