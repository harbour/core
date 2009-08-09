#
# $Id$
#

ROOT := ./

ifeq ($(HB_HOST_BUILD),yes)

DIRS := \
	source \

else

ifeq ($(HB_HOST_BUILD),lib)
   HB_UTIL_DIR :=
else
   HB_UTIL_DIR := utils{source}
endif

DIRS := \
	doc \
	include \
	source \
	$(HB_UTIL_DIR) \
	contrib{source} \
	external \

endif

ifeq ($(HB_POSTINST),)
   ifneq ($(HB_ROOTPOSTINST),)
      HB_POSTINST := $(HB_ROOTPOSTINST) $(HB_POSTINSTPARAM)
   else
      # TOOPT: Use HB_SHELL variable
      ifneq ($(SHLVL),)
         HB_POSTINST = $(TOP)$(ROOT)bin/postinst.sh $(HB_POSTINSTPARAM)
      else
         ifneq ($(OS2_SHELL),)
            HB_POSTINST = $(subst /,\,$(TOP)$(ROOT)bin/postinst.cmd) $(HB_POSTINSTPARAM)
         else
            HB_POSTINST = $(subst /,\,$(TOP)$(ROOT)bin/postinst.bat) $(HB_POSTINSTPARAM)
         endif
      endif
   endif
endif

include $(ROOT)config/dir.cf
