#
# $Id$
#

ifeq ($(HB_MAIN),)
   HB_MAIN := std
endif

ROOT := ../../

include $(TOP)$(ROOT)config/global.cf

ifneq ($(findstring $(HB_COMPILER),gcc djgpp mingw cygwin),)
HB_USER_LDFLAGS += -Wl,--allow-multiple-definition
else
ifneq ($(findstring $(HB_COMPILER),msvc pocc xcc),)
HB_USER_LDFLAGS += /FORCE:MULTIPLE
endif
endif

PRG_SOURCES := \
   t0.prg \
   t1.prg \
   t2.prg \

PRG_MAIN := t0.prg

LIBS := \
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
