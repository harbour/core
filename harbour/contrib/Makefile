#
# $Id$
#

ROOT = ../

ifeq ($(HB_ARCHITECTURE),w32)

DIRS=\
	btree \
	htmllib \
	libct \
	libgt \
	libmisc \
	libnf \
	rdd_ads \
	samples \


else
ifeq ($(HB_COMPILER),icc)

DIRS=\
	hgf/os2pm \
	htmllib \
	libct \
	libgt \
	libmisc \
	libnf \
	samples \

else
ifeq ($(HB_ARCHITECTURE),os2)

DIRS=\
	btree \
	hgf/os2pm \
	htmllib \
	libct \
	libgt \
	libmisc \
	libnf \
	samples \

else

DIRS=\
	btree \
	htmllib \
	libct \
	libgt \
	libmisc \
	libnf \
	samples \

endif
endif
endif

include $(ROOT)config/dir.cf
