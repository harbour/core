#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbqt

PRG_HEADERS := \
   hbqt.ch \

include $(TOP)$(ROOT)contrib/hbqt/detect.mk

ifneq ($(HB_HAS_QT),)

moc_slots.cpp : $(TOP)$(ROOT)contrib/hbqt/hbqt_slots.h
	$(MOC_BIN) $(TOP)$(ROOT)contrib/hbqt/hbqt_slots.h > moc_slots.cpp

   include $(TOP)$(ROOT)contrib/hbqt/filelist.mk

   CPP_SOURCES += \
      hbqt_hbdbfmodel.cpp \
      hbqt_hbdrawingarea.cpp \
      hbqt_hbqmainwindow.cpp \
      hbqt_hbqsyntaxhighlighter.cpp \
      hbqt_hbqtableview.cpp \

   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk

   ifneq ($(filter $(HB_PLATFORM),win wce),)
      ifneq ($(HB_QT_STATIC),)
         DIRS += hbqts
      endif
   endif

   DIRS += \
      qtcore \
      qtgui \
      qtnetwork

   include $(TOP)$(ROOT)config/dir.mk
endif
