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
	asciisum.prg \
	ascpos.prg \
	atadjust.prg \
	attoken.prg \
	atnum.prg \
	atrepl.prg \
	beforatn.prg \
	charadd.prg \
	charand.prg \
	chareven.prg \
	charhist.prg \
	charlist.prg \
	charnlst.prg \
	charnot.prg \
	charodd.prg \
	charor.prg \
	charrll.prg \
	charrlr.prg \
	charshl.prg \
	charshr.prg \
	charslst.prg \
	charsub.prg \
	charxor.prg \
	csetref.prg \
	csetatmu.prg \
	numtoken.prg \
	setatlik.prg \
	token.prg \
	tokenlow.prg \
	tokensep.prg \
	tokenupp.prg \
	valpos.prg \
	

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

