#
# $Id: $
#

ifeq ($(HB_MAIN),)
HB_MAIN = std
endif

HVMLIB = hbvm
ifneq ($(HB_ARCHITECTURE),dos)
ifneq ($(HB_MT),no)
   HVMLIB = hbvmmt
endif
endif

ROOT = ../../

PRG_SOURCES=\
	hbformat.prg \

PRG_MAIN=hbformat.prg

LIBS=\
	hbdebug \
	$(HVMLIB) \
	hbrtl \
	hblang \
	hbcpage \
	hbnulrdd \
	hbrtl \
	$(HVMLIB) \
	hbmacro \
	hbcommon \
	hbpp \
	hbcplr \

include $(TOP)$(ROOT)config/bin.cf
