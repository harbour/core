#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

vpath %.h ../
vpath %.cpp ../
vpath %.prg ../

LIBNAME := gtqtcs

MOC_HEADERS := \
   gtqtc.h \

CPP_SOURCES := \
   gtqtc.cpp \

include $(TOP)$(ROOT)contrib/hbqt/detect.mk

ifneq ($(HB_HAS_QT),)

   HB_CFLAGS += -DQT_NODLL

   include $(TOP)$(ROOT)contrib/hbqt/moc.mk
   include $(TOP)$(ROOT)config/lib.mk
endif
