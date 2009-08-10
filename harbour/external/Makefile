#
# $Id$
#

ROOT := ../

ifeq ($(HB_EXTERNALLIBS),)

# standalone contribs
DIRS := \
   libhpdf \
   libpng \
   sqlite3 \

else

ifneq ($(HB_EXTERNALLIBS),no)
DIRS := $(HB_EXTERNALLIBS)
endif

endif

DIRS += $(HB_EXTERNAL_ADDONS)

ifneq ($(DIRS),)
include $(TOP)$(ROOT)config/dir.cf
else
include $(TOP)$(ROOT)config/none.cf
endif
