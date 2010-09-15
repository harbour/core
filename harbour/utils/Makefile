#
# $Id$
#

ROOT := ../

ifneq ($(HB_BUILD_PARTS),lib)

DIRS := \
   hbrun \
   hbmk2 \

ifneq ($(HB_BUILD_PARTS),compiler)

   DIRS += \
      hbi18n \
      hbtest \
      hbformat \

endif

endif

ifneq ($(DIRS),)
   include $(ROOT)config/dir.mk
else
   include $(ROOT)config/none.mk
endif
