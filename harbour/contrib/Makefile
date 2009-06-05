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
	hbgt \
	hbmisc \
	hbmzip \
	hbnf \
	hbodbc \
	hbsqlit3 \
	hbtip \
	hbtpathy \
	hbvpdf \
	hbwin \
	hbziparc \
	rddado \
	xhb \
	xpp \

# contribs with external dependencies
DIRS +=\
	gtalleg \
	hbblat \
	hbcurl \
	hbfbird \
	hbfimage \
	hbgd \
	hbhpdf \
	hbmsql \
	hbmysql \
	hbpgsql \
	hbqt \
	hbssl \
	rddads \
	rddsql \

else

ifneq ($(HB_CONTRIBLIBS),no)
DIRS = $(HB_CONTRIBLIBS)
endif

endif

DIRS += $(HB_CONTRIB_ADDONS)

ifneq ($(DIRS),)
include $(TOP)$(ROOT)config/dir.cf
else
include $(TOP)$(ROOT)config/none.cf
endif
