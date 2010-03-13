#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbxbp

PRG_SOURCES := \
   hbpprocess.prg \
   xbp3state.prg \
   xbpappevent.prg \
   xbpbitmap.prg \
   xbpbrowse.prg \
   xbpcheckbox.prg \
   xbpcombobox.prg \
   xbpdataref.prg \
   xbpdialog.prg \
   xbpfiledialog.prg \
   xbpfontdialog.prg \
   xbpgeneric.prg \
   xbphtmlviewer.prg \
   xbplistbox.prg \
   xbpmenubar.prg \
   xbpmle.prg \
   xbpparthandler.prg \
   xbppresspace.prg \
   xbpprintdialog.prg \
   xbpprinter.prg \
   xbppushbutton.prg \
   xbpradiobutton.prg \
   xbprtf.prg \
   xbpscrollbar.prg \
   xbpsle.prg \
   xbpspinbutton.prg \
   xbpstatic.prg \
   xbpstatusbar.prg \
   xbpstyle.prg \
   xbptabpage.prg \
   xbptoolbar.prg \
   xbptreeview.prg \
   xbpwindow.prg \
   xbpclipboard.prg \

PRG_HEADERS := \
   appevent.ch \
   gra.ch \
   xbp.ch \
   xbpdev.ch \

# Try to be in sync with hbqt, but not too hard, since this
# lib doesn't need QT external component directly.
ifeq ($(filter $(HB_PLATFORM),dos),)
   ifeq ($(filter $(HB_COMPILER),mingw64 watcom bcc pocc pocc64 poccarm),)
      ifneq ($(HB_WITH_QT),no)

         HB_INC_DEPEND := -I$(TOP)$(ROOT)contrib/hbqt

         include $(TOP)$(ROOT)config/header.mk
         include $(TOP)$(ROOT)config/lib.mk
      else
         HB_SKIP_REASON := component explicitly disabled
         include $(TOP)$(ROOT)config/none.mk
      endif
   else
      HB_SKIP_REASON := compiler not supported
      include $(TOP)$(ROOT)config/none.mk
   endif
else
   HB_SKIP_REASON := platform not supported
   include $(TOP)$(ROOT)config/none.mk
endif
