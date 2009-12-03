#
# $Id$
#

ROOT := ../

ifeq ($(HB_CONTRIBLIBS),)

# contribs _without_ external dependencies
DIRS := \
   gtwvg \
   hbbmcdx \
   hbbtree \
   hbclipsm \
   hbct \
   hbgt \
   hbmemio \
   hbmisc \
   hbmzip \
   hbnetio \
   hbnf \
   hbodbc \
   hbsms \
   hbsqlit3 \
   hbtip \
   hbtpathy \
   hbwin \
   hbziparc \
   xhb \
   xpp \

# contribs _with_ external dependencies
DIRS += \
   gtalleg \
   hbblat \
   hbcairo \
   hbcurl \
   hbfbird \
   hbfimage \
   hbgd \
   hbhpdf \
   hbmysql \
   hbpgsql \
   hbqt \
   hbssl \
   rddads \
   rddsql \

# contribs dependent on above
DIRS += \
   hbxbp \

else

ifneq ($(HB_CONTRIBLIBS),no)
   DIRS := $(HB_CONTRIBLIBS)
else
   DIRS :=
endif

endif

DIRS += $(HB_CONTRIB_ADDONS)

ifneq ($(DIRS),)
include $(TOP)$(ROOT)config/dir.mk
else
include $(TOP)$(ROOT)config/none.mk
endif
