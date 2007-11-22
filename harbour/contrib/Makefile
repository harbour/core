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
	xhb \
	$(HB_CONTRIBLIBS)
#	examples \


ifneq ($(HB_ARCHITECTURE),dos)

DIRS +=\
	bmdbfcdx \

endif

ifeq ($(HB_ARCHITECTURE),w32)

DIRS +=\
	rdd_ads \
	gtwvg \

ifneq ($(HB_COMPILER),rsxnt)

DIRS +=\
	win32 \
	odbc \
	adordd \

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

ifneq ($(HB_ARCHITECTURE),dos)
DIRS += tip
endif

include $(ROOT)config/dir.cf
