#
# $Id$
#

ifeq ($(HB_MAIN),)
HB_MAIN = std
endif

ROOT = ../../../

CONTRIBS=\
	hb_btree\

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

#	debug \
#	vm \
#	rtl \
#	lang \
#	rdd \
#	rtl \
#	vm \
#	macro \
#	pp \
#	common \


ifeq ($(PM),)
  PM := $(pm)
endif

ifeq ($(PM),) # PM not defined = build all files

PRG_SOURCES=\
	test.prg \
	ttest.prg \

PRG_HEADERS=\
	hb_btree.ch

C_HEADERS=\
	hb_btree.api

BAD_PRG_SOURCES=\


C_SOURCES=\
	ctest.c \


C_HEADERS=\


BAD_C_SOURCES=\


###include $(TOP)$(ROOT)config/test.cf
include $(TOP)$(ROOT)config/bin.cf

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

