#
# $Id$
#

ROOT = ../

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
	hbsqlit2 \
	hbsqlit3 \
	hbtip \
	hbtpathy \
	hbvpdf \
	hbw32 \
	hbw32ddr \
	hbwhat32 \
	hbziparch \
	rddado \
	xhb \

# contribs with external dependencies
DIRS +=
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

ifneq ($(HB_CONTRIBLIBS),)
DIRS += $(HB_CONTRIBLIBS)
endif

#DIRS += examples

include $(ROOT)config/dir.cf
