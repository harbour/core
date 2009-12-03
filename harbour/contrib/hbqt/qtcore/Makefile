#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbqtcore

include $(TOP)$(ROOT)contrib/hbqt/detect.mk

ifneq ($(HB_HAS_QT),)

   include $(TOP)$(ROOT)contrib/hbqt/qtcore/filelist.mk

   CPP_SOURCES += QUiLoader.cpp
   PRG_SOURCES += TQUiLoader.prg

   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk

   ifneq ($(filter $(HB_PLATFORM),win wce),)
      ifneq ($(HB_QT_STATIC),)
         DIRS += qtcores
         include $(TOP)$(ROOT)config/dir.mk
      endif
   endif
endif
