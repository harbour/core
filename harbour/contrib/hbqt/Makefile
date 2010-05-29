#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbqt

PRG_HEADERS := \
   hbqt.ch \

_QT_HEADERS = qtgui qtnetwork

include $(TOP)$(ROOT)contrib/hbqt/detect.mk

ifneq ($(HB_HAS_QT),)

   include $(TOP)$(ROOT)contrib/hbqt/filelist.mk

   include $(TOP)$(ROOT)contrib/hbqt/moc.mk
   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk

   ifneq ($(HB_QT_STATIC),)
      DIRS += hbqts
   endif

   DIRS += \
      qtcore \
      qtgui \
      qtnetwork

   include $(TOP)$(ROOT)config/dir.mk
endif
