#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtqtc

MOC_HEADERS := \
   gtqtc.h \

CPP_SOURCES := \
   gtqtc.cpp \
   moc_gtqtc.cpp \

PRG_HEADERS := \
   hbgtqtc.ch

_QT_HEADERS = qtgui

include $(TOP)$(ROOT)contrib/hbqt/detect.mk

ifneq ($(HB_HAS_QT),)

   include $(TOP)$(ROOT)contrib/hbqt/moc.mk
   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk

   ifneq ($(filter $(HB_PLATFORM),win wce),)
      ifneq ($(HB_QT_STATIC),)
         DIRS += gtqtcs
         include $(TOP)$(ROOT)config/dir.mk
      endif
   endif
endif
