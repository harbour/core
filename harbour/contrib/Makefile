#
# $Id$
#

ROOT = ../

ifeq ($(HB_ARCHITECTURE),w32)

DIRS=\
	apollo \
	htmllib \
	libct \
	libgt \
	libmisc \
	rdd_ads \


else
ifeq ($(HB_ARCHITECTURE),os2)

DIRS=\
	hgf/os2pm \
	htmllib \
	libct \
	libgt \
	libmisc \

else

DIRS=\
	htmllib \
	libct \
	libgt \
	libmisc \

endif
endif

include $(ROOT)config/dir.cf
