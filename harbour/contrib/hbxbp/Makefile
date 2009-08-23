#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

HB_INC_DEPEND := -I$(TOP)$(ROOT)contrib/hbqt

LIBNAME := hbxbp

ifeq ($(HB_WITH_QT),)
   HB_WITH_QT := yes
   ifeq ($(HB_PLATFORM),dos)
      HB_WITH_QT := no
   endif
   ifeq ($(HB_PLATFORM),os2)
      HB_WITH_QT := no
   endif
   ifeq ($(HB_COMPILER),mingw64)
      HB_WITH_QT := no
   endif
   ifeq ($(HB_COMPILER),watcom)
      HB_WITH_QT := no
   endif
   ifeq ($(HB_COMPILER),bcc)
      HB_WITH_QT := no
   endif
   ifeq ($(HB_COMPILER),pocc)
      HB_WITH_QT := no
   endif
   ifeq ($(HB_COMPILER),pocc64)
      HB_WITH_QT := no
   endif
   ifeq ($(HB_COMPILER),poccarm)
      HB_WITH_QT := no
   endif
endif

ifeq ($(HB_WITH_QT),yes)

PRG_SOURCES := \
   xbpgeneric.prg \
   xbpdialog.prg \
   xbpwindow.prg \
   xbpparthandler.prg \
   xbpmenubar.prg \
   xbptoolbar.prg \
   xbppushbutton.prg \
   xbpdataref.prg \
   xbpcheckbox.prg \
   xbp3state.prg \
   xbpradiobutton.prg \
   xbptabpage.prg \
   xbplistbox.prg \
   xbpstatusbar.prg \
   xbpscrollbar.prg \
   xbpsle.prg \
   xbpmle.prg \
   xbpspinbutton.prg \
   xbpcombobox.prg \
   xbptreeview.prg \
   xbpstyle.prg \
   xbpappevent.prg \
   xbpstatic.prg \
   xbphtmlviewer.prg \
   xbpfiledialog.prg \
   xbpfontdialog.prg \
   xbpbitmap.prg \
   xbprtf.prg \
   xbppresspace.prg \
   xbpprinter.prg \
   xbpprintdialog.prg \
   xbpbrowse.prg \

PRG_HEADERS := \
   xbp.ch \
   xbpdev.ch \
   appevent.ch \
   gra.ch \

include $(TOP)$(ROOT)config/header.mk
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.mk

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.mk
endif
