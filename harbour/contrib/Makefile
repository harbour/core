#
# $Id$
#

ROOT = ../

DIRS=\
	btree \
	htmllib \
	libct \
	libgt \
	libmisc \
	libnf \
	samples \


ifeq ($(HB_ARCHITECTURE),w32)

DIRS +=\
	rdd_ads \

ifeq ($(HB_COMPILER),msvc)

DIRS +=\
	odbc \

else
ifeq ($(HB_COMPILER),rsxnt)

else

DIRS +=\
	ole \
	odbc \

endif
endif

else
ifeq ($(HB_COMPILER),icc)

DIRS +=\
	hgf/os2pm \

else
ifeq ($(HB_ARCHITECTURE),os2)

DIRS +=\
	hgf/os2pm \

else
ifeq ($(HB_ARCHITECTURE),linux)

DIRS +=\
	rdd_ads \
	odbc \

endif
endif
endif
endif

include $(ROOT)config/dir.cf
