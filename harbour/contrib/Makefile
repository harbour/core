#
# $Id$
#

ROOT = ../

ifeq ($(HB_ARCHITECTURE),w32)

ifeq ($(HB_COMPILER),msvc)

DIRS=\
	htmllib \
	libct \
	libgt \
	libmisc \
	rdd_ads \
	samples \

else

DIRS=\
	btree \
	htmllib \
	libct \
	libgt \
	libmisc \
	libnf \
	rdd_ads \
	ole \
	odbc \
	samples \

endif

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
	htmllib \
	libct \
	libgt \
	libmisc \
	rdd_ads \
	odbc \
	samples \
 
endif
endif
endif

include $(ROOT)config/dir.cf
