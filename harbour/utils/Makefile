#
# $Id$
#

ROOT := ../

ifneq ($(HB_BUILD_PARTS),lib)

DIRS := \
   hbmk2 \

ifneq ($(HB_BUILD_PARTS),compiler)

   DIRS += \
      hbi18n \
      hbrun \
      hbtest \
      $(__HB_UTIL_ADDONS)

endif

endif

ifneq ($(DIRS),)
   include $(ROOT)config/dir.mk
else
   include $(ROOT)config/none.mk
endif
