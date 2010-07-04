#
# $Id$
#

ROOT := ./

include $(ROOT)config/global.mk

ifeq ($(HB_BUILD_PARTS),compiler)

   DIRS := \
      src \
      utils{src} \

else

   DIRS := \
      doc \
      include \
      external \
      src{external} \
      utils{src} \
      contrib{src} \

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
	$(if $(wildcard $(HB_HOST_BIN_DIR)/hbrun$(HB_HOST_BIN_EXT)),+$(HB_HOST_BIN_DIR)/hbrun$(HB_HOST_BIN_EXT) --hb:gtcgi $(TOP)$(ROOT)config/postinst.hbs,)
	$(if $(wildcard $(HB_POSTINST)),+$(HB_POSTINST),)
