#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

vpath %.cpp ../
vpath %.prg ../

LIBNAME := hbqts

include $(TOP)$(ROOT)contrib/hbqt/detect.mk

ifneq ($(HB_HAS_QT),)

   HB_CFLAGS += -DQT_NODLL

moc_slots.cpp : $(TOP)$(ROOT)contrib/hbqt/hbqt_slots.h
	$(MOC_BIN) $(TOP)$(ROOT)contrib/hbqt/hbqt_slots.h > moc_slots.cpp

   include $(TOP)$(ROOT)contrib/hbqt/filelist.mk

   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk
endif
