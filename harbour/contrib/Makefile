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
	xhb \
	$(HB_CONTRIBLIBS)
#	examples \


ifneq ($(HB_ARCHITECTURE),dos)

DIRS +=\
	hbbmcdx \

endif

ifeq ($(HB_ARCHITECTURE),w32)

DIRS +=\
	hbrddads \
	hbgtwvg \

ifneq ($(HB_COMPILER),rsxnt)

DIRS +=\
	hbw32 \
	hbodbc \
	hbrddado \

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
	hbrddads \
	hbodbc \

endif
endif
endif
endif

ifneq ($(HB_ARCHITECTURE),dos)
DIRS += hbtip
endif

include $(ROOT)config/dir.cf
