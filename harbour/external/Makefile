#
# $Id$
#

ROOT := ../

ifeq ($(HB_EXTERNALLIBS),)

# standalone contribs
DIRS := \
   libhpdf \
   libpng \
   pcre \
   sqlite3 \
   zlib \

else

ifneq ($(HB_EXTERNALLIBS),no)
   DIRS := $(HB_EXTERNALLIBS)
else
   DIRS :=
endif

endif

DIRS += $(HB_EXTERNAL_ADDONS)

ifneq ($(DIRS),)
include $(TOP)$(ROOT)config/dir.mk
else
include $(TOP)$(ROOT)config/none.mk
endif
