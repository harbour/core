#
# $Id$
#

ROOT = ../

DIRS=\
	hbbtree \
	hbct \
	hbgt \
	hbmisc \
	hbnf \
	hbclipsm \
	hbvpdf \
	xhb \
	hbmzip \
#	examples \


ifneq ($(HB_ARCHITECTURE),dos)
DIRS += hbbmcdx
endif

ifeq ($(HB_ARCHITECTURE),w32)
DIRS += rddads gtwvg
ifneq ($(HB_COMPILER),rsxnt)
DIRS += hbw32 hbodbc rddado
endif
endif

ifeq ($(HB_COMPILER),icc)
DIRS += hbgf/os2pm
else
ifeq ($(HB_ARCHITECTURE),os2)
DIRS += hbgf/os2pm
endif
endif

ifeq ($(HB_ARCHITECTURE),linux)
ifneq ($(HB_WITHOUT_ODBC),yes)
DIRS += hbodbc
endif
ifneq ($(HB_WITHOUT_ADS),yes)
DIRS += rddads
endif
endif

ifneq ($(HB_ARCHITECTURE),dos)
DIRS += hbtip
endif

ifneq ($(HB_CONTRIBLIBS),)
DIRS += $(HB_CONTRIBLIBS)
endif

include $(ROOT)config/dir.cf
