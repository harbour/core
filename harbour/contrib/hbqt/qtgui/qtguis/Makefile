#
# $Id$
#

ROOT := ../../../../

include $(TOP)$(ROOT)config/global.mk

vpath %.h ../
vpath %.cpp ../
vpath %.prg ../

LIBNAME := hbqtguis

_QT_HEADERS = qtgui

include $(TOP)$(ROOT)contrib/hbqt/detect.mk

ifneq ($(HB_HAS_QT),)

   HB_CFLAGS += -DQT_NODLL

   include $(TOP)$(ROOT)contrib/hbqt/qtgui/filelist.mk

   include $(TOP)$(ROOT)contrib/hbqt/moc.mk
   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := $(_DET_RES_TEXT)
   include $(TOP)$(ROOT)config/none.mk
endif
