#
# $Id$
#

ROOT = ../../

LIBS=\
        tools \
        debug \
        rtl \
        rdd \
        vm \
        rdd \
        rtl \
        hbpp \
        runner \

ifeq ($(PM),)
  PM := $(pm)
endif
ifeq ($(PM),)   # PM not defined = build all files
PRG_SOURCES=\
huh.prg \
rtl_test.prg \
fileio.prg \
and_or.prg \
copyfile.prg \
1.prg \
2.prg \
kej.prg \
kejj.prg \
inkeytst.prg \
para.prg \
rtl_test.prg \
i.prg \
j.prg \
k.prg \
ac_test.prg \
fileio.prg \
rtl_test.prg \
testrdd.prg \
testdbf.prg \

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
  PRG_MAIN :=$(PM)
else
  ifneq ($(findstring .PRG,$(PM)),)
    PRG_MAIN := $(PM)
  else
    PRG_MAIN := $(PM).prg
  endif
endif
include $(TOP)$(ROOT)config/bin.cf

endif
