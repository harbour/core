#
# $Id$
#

ROOT = ../

LIBS=\
	tools \
	debug \
	rtl \
        rdd \
        vm \
        rdd \
        rtl \
	macro \
        pp \
	runner \
	common \

ifeq ($(PM),)
  PM := $(pm)
endif
ifeq ($(PM),)   # PM not defined = build all files

DIRS=\
        regress \

include $(TOP)$(ROOT)config/dir.cf

PRG_SOURCES=\
a.prg \

#memory.prg \
#testbrdb.prg \
#tstmacro.prg \
#debugtst.prg \
#dynsym.prg \
#debugtst.prg \
#memfile.prg \
#adirtest.prg \
#overload.prg \
#statinit.prg \
#ac_test.prg \
#fortest.prg \
#and_or.prg \
#inkeytst.prg \
#testrdd.prg \
#testdbf.prg \

PRG_HEADERS=\
        cgi.ch \
        keywords.ch \
        test.ch \

BAD_PRG_SOURCES=\
        alias.prg \
        dupvars.prg \
        extend1.prg \
        keywords.prg \
        linecont.prg \
        setkeys.prg \
        spawn.prg \
        spawn2.prg \
        statics1.prg \
        statics2.prg \
        test10.prg \
        testid.prg \

C_SOURCES=\

C_HEADERS=\

BAD_C_SOURCES=\
        extend2.c \

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
