#
# $Id$
#

ifeq ($(HB_MAIN),)
HB_MAIN = std
endif

ROOT = ../../../

CONTRIBS=\
	libct\

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
	common \

ifeq ($(PM),)
  PM := $(pm)
endif

ifeq ($(PM),) # PM not defined = build all files

PRG_SOURCES=\
	addascii.prg \
	afteratn.prg \
	atnum.prg \
	beforatn.prg \
	csetref.prg \
	csetatmu.prg \
	setatlik.prg \
	

PRG_HEADERS=\


BAD_PRG_SOURCES=\


C_SOURCES=\


C_HEADERS=\


BAD_C_SOURCES=\


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

