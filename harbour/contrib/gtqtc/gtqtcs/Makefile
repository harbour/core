#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

vpath %.cpp ../
vpath %.prg ../

LIBNAME := gtqtcs

CPP_SOURCES := \
   gtqtc.cpp \
   moc_gtqtc.cpp \

PRG_HEADERS := \
   hbgtqtc.ch

_QT_HEADERS = qtgui

include $(TOP)$(ROOT)contrib/hbqt/detect.mk

ifneq ($(HB_HAS_QT),)

   HB_CFLAGS += -DQT_NODLL

moc_gtqtc.cpp : $(TOP)$(ROOT)contrib/gtqtc/gtqtc.h
	$(MOC_BIN) $(TOP)$(ROOT)contrib/gtqtc/gtqtc.h > moc_gtqtc.cpp

   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk
endif
