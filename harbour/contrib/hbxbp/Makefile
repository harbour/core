#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbxbp

HB_INC_DEPEND := -I$(TOP)$(ROOT)contrib/hbqt

ifneq ($(filter $(HB_PLATFORM),dos os2),)
   PREREQ_MISS := platform
else ifneq ($(filter $(HB_COMPILER),mingw64 watcom bcc pocc pocc64 poccarm),)
   PREREQ_MISS := compiler
else
   PREREQ_MISS :=
endif

ifeq ($(PREREQ_MISS),)

   ifneq ($(HB_INC_QT),no)

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
      HB_SKIP_REASON := deselected
      include $(TOP)$(ROOT)config/none.mk
   endif
else
   HB_SKIP_REASON := $(PREREQ_MISS) not supported
   include $(TOP)$(ROOT)config/none.mk
endif
