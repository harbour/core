#
# $Id$
#

ROOT = ../

ifeq ($(HB_ARCHITECTURE),w32)

DIRS=\
	htmllib \
	libct \
	libgt \
	libmisc \
	libnf \
	rdd_ads \
	samples \


else
ifeq ($(HB_ARCHITECTURE),os2)

DIRS=\
	hgf/os2pm \
	htmllib \
	libct \
	libgt \
	libmisc \
	libnf \
	samples \

else

DIRS=\
	htmllib \
	libct \
	libgt \
	libmisc \
	libnf \
	samples \

endif
endif

include $(ROOT)config/dir.cf
