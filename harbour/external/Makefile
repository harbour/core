#
# $Id$
#

ROOT := ../

DIRS := \
   sqlite3 \
   bzip2 \
   libhpdf \
   hbpmcom \
   png \
   jpeg \
   pcre \
   zlib \
   minizip \

ifeq ($(firstword $(HB_EXTERNALLIBS)),no)
   DIRS := $(filter-out $(HB_EXTERNALLIBS),$(DIRS))
else
ifneq ($(HB_EXTERNALLIBS),)
   DIRS := $(HB_EXTERNALLIBS)
endif
endif

ifneq ($(DIRS),)
   include $(TOP)$(ROOT)config/dir.mk
else
   include $(TOP)$(ROOT)config/none.mk
endif
