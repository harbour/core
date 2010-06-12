#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtqtc

MOC_HEADERS := \
   gtqtc.h \

CPP_SOURCES := \
   gtqtc.cpp \

PRG_HEADERS := \
   hbgtqtc.ch

include $(TOP)$(ROOT)contrib/hbqt/detect.mk

ifneq ($(HB_HAS_QT),)

   include $(TOP)$(ROOT)contrib/hbqt/moc.mk
   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk

   ifneq ($(HB_QT_STATIC),)
      ifneq ($(filter $(HB_PLATFORM),win wce),)
         DIRS += gtqtcs
         include $(TOP)$(ROOT)config/dir.mk
      endif
   endif
endif
