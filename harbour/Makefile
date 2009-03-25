#
# $Id$
#

ROOT = ./

ifeq ($(HB_HOST_BUILD),yes)

DIRS=\
	source \

else

ifeq ($(HB_HOST_BUILD),lib)
HB_UTIL_DIR=
else
HB_UTIL_DIR=utils{source}
endif

DIRS=\
	doc \
	include \
	source \
	$(HB_UTIL_DIR) \
	contrib{source} \
	external \

endif

ifeq ($(HB_POSTINST),)

ifneq ($(HB_ROOTPOSTINST),)
HB_POSTINST = $(HB_ROOTPOSTINST) $(HB_POSTINSTPARAM)
else
ifneq ($(SHLVL),)
HB_POSTINST = $(TOP)$(ROOT)bin/postinst.sh $(HB_POSTINSTPARAM)
else
ifeq ($(HB_ARCHITECTURE),dos)
HB_POSTINST = $(subst /,\,$(TOP)$(ROOT)bin/postinst.bat) $(HB_POSTINSTPARAM)
else
ifeq ($(HB_ARCHITECTURE),win)
HB_POSTINST = $(subst /,\,$(TOP)$(ROOT)bin/postinst.bat) $(HB_POSTINSTPARAM)
else
ifeq ($(HB_ARCHITECTURE),os2)
HB_POSTINST = $(subst /,\,$(TOP)$(ROOT)bin/postinst.cmd) $(HB_POSTINSTPARAM)
endif
endif
endif
endif
endif

endif

include $(ROOT)config/dir.cf
