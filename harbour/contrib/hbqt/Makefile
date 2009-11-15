#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbqt

PRG_HEADERS := \
   hbqt.ch \

PRG_SOURCES := \
   hbqt_misc.prg

include $(TOP)$(ROOT)contrib/hbqt/detect.mk

ifneq ($(HB_HAS_QT),)

   include $(TOP)$(ROOT)contrib/hbqt/filelist.mk

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
      qtnetwork \
      qtwebkit    

   include $(TOP)$(ROOT)config/dir.mk
endif
