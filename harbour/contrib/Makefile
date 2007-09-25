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
	$(HB_CONTRIBLIBS)


ifneq ($(HB_ARCHITECTURE),dos)

DIRS +=\
	bmdbfcdx \

endif

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
	ole2 \
	odbc \
	adordd \

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

#ifneq ($(HB_ARCHITECTURE),dos)
#DIRS += tip
#DIRS += xhb
#endif

include $(ROOT)config/dir.cf
