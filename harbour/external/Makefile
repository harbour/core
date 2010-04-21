#
# $Id$
#

ROOT := ../

# standalone contribs
DIRS := \
   sqlite3 \
   bzip2 \
   libhpdf \
   libpng \
   jpeg \
   pcre \
   zlib \
   minizip \

ifeq ($(firstword $(HB_EXTERNALLIBS)),no)
   DIRS := $(filter-out $(HB_EXTERNALLIBS),$(DIRS))
else ifneq ($(HB_EXTERNALLIBS),)
   DIRS := $(HB_EXTERNALLIBS)
endif

DIRS += $(HB_EXTERNAL_ADDONS)

ifneq ($(DIRS),)
include $(TOP)$(ROOT)config/dir.mk
else
include $(TOP)$(ROOT)config/none.mk
endif
