#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtqtc

CPP_SOURCES := \
   gtqtc.cpp \
   moc_gtqtc.cpp \

PRG_HEADERS := \
   hbgtqtc.ch

include $(TOP)$(ROOT)contrib/hbqt/detect.mk

ifneq ($(HB_HAS_QT),)

moc_gtqtc.cpp : $(TOP)$(ROOT)contrib/gtqtc/gtqtc.h
	$(MOC_BIN) $(TOP)$(ROOT)contrib/gtqtc/gtqtc.h > moc_gtqtc.cpp

   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk

   ifneq ($(filter $(HB_PLATFORM),win wce),)
      ifneq ($(HB_QT_STATIC),)
         DIRS += gtqtcs
         include $(TOP)$(ROOT)config/dir.mk
      endif
   endif
endif
