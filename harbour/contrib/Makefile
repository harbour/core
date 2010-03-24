#
# $Id$
#

ROOT := ../

# contribs _without_ external dependencies
DIRS := \
   gtwvg \
   hbbtree \
   hbbz2 \
   hbclipsm \
   hbct \
   hbfoxpro \
   hbfship \
   hbgt \
   hbmemio \
   hbmisc \
   hbmzip \
   hbnetio \
   hbnf \
   hbodbc \
   hbsqlit3 \
   hbtip \
   hbtpathy \
   hbwin \
   hbxpp \
   hbziparc \
   rddbmcdx \
   xhb \

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
   sddmy \
   sddpg \
   sddfb \
   sddoci \
   sddodbc \
   sddsqlt3 \

# contribs dependent on above
DIRS += \
   hbxbp \

ifeq ($(HB_CONTRIBLIBS),no)
   DIRS :=
else ifeq ($(firstword $(HB_CONTRIBLIBS)),no)
   DIRS := $(filter-out $(HB_CONTRIBLIBS),$(DIRS))
else ifneq ($(HB_CONTRIBLIBS),)
   DIRS := $(HB_CONTRIBLIBS)
endif

DIRS += $(HB_CONTRIB_ADDONS)

ifneq ($(DIRS),)
   include $(TOP)$(ROOT)config/dir.mk
else
   include $(TOP)$(ROOT)config/none.mk
endif
