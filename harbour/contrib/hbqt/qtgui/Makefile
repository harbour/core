#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbqtgui

# force redetect. needed for _QT_DARWIN trick
HB_HAS_QT :=

_DET_DSP_NAME := qt
_DET_VAR_INC_ := HB_INC_QT
_DET_VAR_HAS_ := HB_HAS_QT
_DET_FLT_PLAT := !dos !os2
_DET_FLT_COMP := !mingw64 !watcom !bcc !pocc !pocc64 !poccarm !msvcia64
_DET_INC_DEFP := /usr/include/qt4 /usr/lib/qt4/include /usr/include /Developer/qt/include
_DET_INC_HEAD := /Qt/qglobal.h
include $(TOP)$(ROOT)config/detfun.mk

_QT_DARWIN :=
ifeq ($(HB_PLATFORM),darwin)
   ifeq ($(HB_HAS_QT),)
      _DET_DSP_NAME := qt
      _DET_VAR_INC_ := HB_INC_QT
      _DET_VAR_HAS_ := HB_HAS_QT
      _DET_INC_DEFP := /Library/Frameworks/QtCore.framework/Versions/4/Headers
      _DET_INC_HEAD := /QtCore
      include $(TOP)$(ROOT)config/detfun.mk
      _QT_DARWIN := yes
   endif
endif

ifneq ($(HB_HAS_QT),)

   ifeq ($(_QT_DARWIN),yes)
      HB_CFLAGS += -I/Library/Frameworks/QtCore.framework/Headers
      HB_CFLAGS += -I/Library/Frameworks/QtGui.framework/Headers
      HB_CFLAGS += -I/Library/Frameworks/QtNetwork.framework/Headers
      HB_CFLAGS += -I/Library/Frameworks/QtWebKit.framework/Headers
   else
      HB_CFLAGS += $(foreach d,$(HB_HAS_QT),-I$(d) -I$(d)/Qt -I$(d)/QtCore -I$(d)/QtGui -I$(d)/QtNetwork -I$(d)/QtWebKit)
   endif

   include $(TOP)$(ROOT)contrib/hbqt/qtgui/filelist.mk

   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk

   ifneq ($(filter $(HB_PLATFORM),win wce),)
      ifneq ($(HB_QT_STATIC),)
         DIRS += qtguis
         include $(TOP)$(ROOT)config/dir.mk
      endif
   endif
else
   HB_SKIP_REASON := $(_DET_RES_TEXT)
   include $(TOP)$(ROOT)config/none.mk
endif
