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

   MOC_HEADERS := \
      hbqt_events.h \
      hbqt_hbdbfmodel.h \
      hbqt_hbdrawingarea.h \
      hbqt_hbqmainwindow.h \
      hbqt_hbqsyntaxhighlighter.h \
      hbqt_hbqtableview.h \
      hbqt_slots.h \
	            
   CPP_SOURCES := \
      hbqt_base.cpp \
      hbqt_utils.cpp \
      hbqt_slots.cpp \
      hbqt_destruct.cpp \
      hbqt_events.cpp \
      hbqt_hbdbfmodel.cpp \
      hbqt_hbdrawingarea.cpp \
      hbqt_hbqmainwindow.cpp \
      hbqt_hbqsyntaxhighlighter.cpp \
      hbqt_hbqtableview.cpp \

MOC_CPP_SOURCES := $(foreach dir,$(MOC_HEADERS),moc_$(dir:.h=.cpp))

$(MOC_CPP_SOURCES) : moc_%.cpp : $(GRANDP)%.h

moc_%.cpp : $(GRANDP)%.h
	$(MOC_BIN) $? -o $@

   CPP_SOURCES += $(MOC_CPP_SOURCES)

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
