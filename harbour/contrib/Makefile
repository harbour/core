#
# $Id$
#

ROOT = ../

DIRS=\
	gtwvg \
	hbbmcdx \
	hbbtree \
	hbct \
	hbgt \
	hbmisc \
	hbnf \
	hbclipsm \
	hbvpdf \
	hbmzip \
	hbtip \
	xhb \
#	examples \

ifeq ($(HB_ARCHITECTURE),w32)
DIRS += rddads
ifneq ($(HB_COMPILER),rsxnt)
DIRS += hbw32 hbodbc rddado
endif
endif

ifeq ($(HB_ARCHITECTURE),os2)
DIRS += hbgf/os2pm
endif

ifeq ($(HB_ARCHITECTURE),linux)
DIRS += rddads hbodbc
endif

ifneq ($(HB_CONTRIBLIBS),)
DIRS += $(HB_CONTRIBLIBS)
endif

include $(ROOT)config/dir.cf
