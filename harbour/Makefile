#
# $Id$
#

ROOT := ./

ifeq ($(HB_BUILD_PART),compiler)

   DIRS := \
      source \

else

   ifeq ($(HB_BUILD_PART),lib)
      HB_UTIL_DIR :=
   else
      HB_UTIL_DIR := utils{source}
   endif

   DIRS := \
      doc \
      include \
      external \
      source{external} \
      $(HB_UTIL_DIR) \
      contrib{source} \

endif

include $(ROOT)config/dir.mk

ifeq ($(HB_SHELL),sh)
   HB_POSTINST := $(TOP)$(ROOT)bin/postinst.sh
else
   ifeq ($(HB_SHELL),os2)
      HB_POSTINST := $(subst /,\,$(TOP)$(ROOT)bin/postinst.cmd)
   else
      HB_POSTINST := $(subst /,\,$(TOP)$(ROOT)bin/postinst.bat)
   endif
endif

install::
	+$(HB_POSTINST)
