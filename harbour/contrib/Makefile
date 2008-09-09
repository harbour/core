#
# $Id$
#

ROOT = ../

ifeq ($(HB_CONTRIBLIBS),)

# standalone contribs
DIRS=\
	gtwvg \
	hbbmcdx \
	hbbtree \
	hbclipsm \
	hbct \
	hbgf \
	hbgt \
	hbmisc \
	hbmzip \
	hbnf \
	hbodbc \
	hbole \
	hbsqlit3 \
	hbtip \
	hbtpathy \
	hbvpdf \
	hbw32 \
	hbwhat32 \
	hbziparc \
	rddado \
	xhb \

# contribs with external dependencies
DIRS +=\
	gtalleg \
	hbapollo \
	hbcurl \
	hbfbird \
	hbfimage \
	hbgd \
	hbhpdf \
	hbmsql \
	hbmysql \
	hbpgsql \
	rddads \

else

DIRS = $(HB_CONTRIBLIBS)

endif

DIRS += $(HB_CONTRIB_ADDONS)

# DIRS += examples

include $(ROOT)config/dir.cf
