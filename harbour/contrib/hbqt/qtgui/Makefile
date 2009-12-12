#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbqtgui

_QT_HEADERS = qtgui

include $(TOP)$(ROOT)contrib/hbqt/detect.mk

ifneq ($(HB_HAS_QT),)

   include $(TOP)$(ROOT)contrib/hbqt/qtgui/filelist.mk

   include $(TOP)$(ROOT)contrib/hbqt/moc.mk
   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk

   ifneq ($(filter $(HB_PLATFORM),win wce),)
      ifneq ($(HB_QT_STATIC),)
         DIRS += qtguis
         include $(TOP)$(ROOT)config/dir.mk
      endif
   endif
endif
