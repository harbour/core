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
	hbcrypt \
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
	hbwin \
#	hbwhat \
	hbziparc \
	rddado \
	xhb \

# contribs with external dependencies
DIRS +=\
	gtalleg \
	hbapollo \
	hbblat \
	hbcurl \
	hbfbird \
	hbfimage \
	hbgd \
	hbhpdf \
	hbmsql \
	hbmysql \
	hbpgsql \
	hbssl \
	rddads \
	rddsql \

else

DIRS = $(HB_CONTRIBLIBS)

endif

DIRS += $(HB_CONTRIB_ADDONS)

# DIRS += examples

include $(ROOT)config/dir.cf
