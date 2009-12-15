#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

vpath %.h ../
vpath %.cpp ../
vpath %.prg ../

LIBNAME := hbqts

_QT_HEADERS = qtgui qtnetwork

include $(TOP)$(ROOT)contrib/hbqt/detect.mk

ifneq ($(HB_HAS_QT),)

   HB_CFLAGS += -DQT_NODLL

   include $(TOP)$(ROOT)contrib/hbqt/filelist.mk

   include $(TOP)$(ROOT)contrib/hbqt/moc.mk
   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk
endif
