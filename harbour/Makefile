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

   # When doing a plain clean, we must not clean hbrun and hbmk2
   # before calling it to clean the contrib area.
   _CONTRIB_FIRST :=
   ifneq ($(filter clean,$(HB_MAKECMDGOALS)),)
      ifeq ($(filter install,$(HB_MAKECMDGOALS)),)
         _CONTRIB_FIRST := yes
      endif
   endif

   DIRS := \
      doc \
      include \
      src \

   ifeq ($(_CONTRIB_FIRST),yes)

      DIRS += \
         contrib{src} \
         utils{contrib} \

   else

      DIRS += \
         utils{src} \
         contrib{utils} \

   endif
endif

include $(ROOT)config/dir.mk

ifneq ($(HB_NO_HBSCRIPT),yes)

first clean install::
	$(if $(wildcard $(HB_HOST_BIN_DIR)/hbrun$(HB_HOST_BIN_EXT)),+$(HB_HOST_BIN_DIR)/hbrun$(HB_HOST_BIN_EXT) --hb:gtcgi $(TOP)$(ROOT)config/postinst.hbs $@,@$(ECHO) $(ECHOQUOTE)! Warning: hbrun not found, config/postinst.hbs skipped.$(ECHOQUOTE))

endif
