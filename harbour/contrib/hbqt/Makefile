#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbqt

ifneq ($(filter $(HB_PLATFORM),dos os2),)
   PREREQ_MISS := platform
else ifneq ($(filter $(HB_COMPILER),mingw64 watcom bcc pocc pocc64 poccarm),)
   PREREQ_MISS := compiler
else
   PREREQ_MISS :=
endif

ifeq ($(PREREQ_MISS),)

   ifneq ($(HB_INC_QT),no)

      ifeq ($(HB_INC_QT),)
         ifeq ($(HB_XBUILD),)
            ifeq ($(HB_OS_UNIX),yes)
               HB_INC_QT := /usr/include/qt4 /usr/lib/qt4/include /usr/include /Developer/qt/include
               ifeq ($(HB_PLATFORM),darwin)
                  HB_INC_QT += /Library/Frameworks/QtCore.framework/Versions/4/Headers/QtCore
               endif
            endif
         endif
      endif

      ifneq ($(HB_INC_QT),)

         HB_INC_QT_OK := $(strip $(foreach d, $(HB_INC_QT), $(if $(wildcard $(d)/Qt/qglobal.h),$(d),)))

         ifneq ($(HB_INC_QT_OK),)

            ifneq ($(HB_QT_STATIC),)
               LIBNAME := hbqts
               HB_CFLAGS += -DQT_NODLL
            endif

            ifeq ($(HB_INC_QT_OK),/Library/Frameworks/QtCore.framework/Versions/4/Headers/QtCore)
               HB_CFLAGS += -I/Library/Frameworks/QtCore.framework/Headers
               HB_CFLAGS += -I/Library/Frameworks/QtGui.framework/Headers
               HB_CFLAGS += -I/Library/Frameworks/QtNetwork.framework/Headers
               HB_CFLAGS += -I/Library/Frameworks/QtWebKit.framework/Headers
            else
               HB_CFLAGS += $(foreach d,$(HB_INC_QT_OK),-I$(d) -I$(d)/Qt -I$(d)/QtCore -I$(d)/QtGui -I$(d)/QtNetwork -I$(d)/QtWebKit)
            endif

            include $(TOP)$(ROOT)contrib/hbqt/filelist.mk

            PRG_HEADERS := \
               hbqt.ch \

            include $(TOP)$(ROOT)config/header.mk
            INSTALL_RULE_HEADERS := $(INSTALL_RULE)
            include $(TOP)$(ROOT)config/lib.mk

            install::
	$(INSTALL_RULE_HEADERS)

         else
            HB_SKIP_REASON := component not found
            include $(TOP)$(ROOT)config/none.mk
         endif
      else
         HB_SKIP_REASON := component location not set
         include $(TOP)$(ROOT)config/none.mk
      endif
   else
      HB_SKIP_REASON := deselected
      include $(TOP)$(ROOT)config/none.mk
   endif
else
   HB_SKIP_REASON := $(PREREQ_MISS) not supported
   include $(TOP)$(ROOT)config/none.mk
endif
