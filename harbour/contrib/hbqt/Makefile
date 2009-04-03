#
# $Id$
#

ROOT = ../../

LIBNAME=hbqt

ifeq ($(HB_WITH_QT),)
HB_WITH_QT=yes
ifeq ($(HB_ARCHITECTURE),dos)
HB_WITH_QT=no
endif
ifeq ($(HB_ARCHITECTURE),os2)
HB_WITH_QT=no
endif
ifeq ($(HB_COMPILER),mingw64)
HB_WITH_QT=no
endif
ifeq ($(HB_COMPILER),owatcom)
HB_WITH_QT=no
endif
ifeq ($(HB_COMPILER),bcc)
HB_WITH_QT=no
endif
ifeq ($(HB_COMPILER),pocc)
HB_WITH_QT=no
endif
ifeq ($(HB_COMPILER),pocc64)
HB_WITH_QT=no
endif
ifeq ($(HB_COMPILER),poccce)
HB_WITH_QT=no
endif
ifeq ($(HB_COMPILER),poccarm)
HB_WITH_QT=no
endif
endif

ifeq ($(HB_WITH_QT),yes)

ifeq ($(HB_INC_QT),)
ifeq ($(HB_XBUILD),)
HB_INC_QT = /usr/include/qt4 /Developer/qt/include
endif
endif

HB_INC_QT_OK += $(foreach d, $(HB_INC_QT), $(if $(wildcard $(d)/Qt/qglobal.h),$(d),))

ifneq ($(strip $(HB_INC_QT_OK)),)

HB_USER_CFLAGS += $(foreach d, $(HB_INC_QT_OK), -I$(d))

CPP_SOURCES=\
	hbqt_qabstractbutton.cpp \
	hbqt_qabstractitemview.cpp \
	hbqt_qabstractprintdialog.cpp \
	hbqt_qabstractscrollarea.cpp \
	hbqt_qabstractslider.cpp \
	hbqt_qabstractspinbox.cpp \
	hbqt_qaction.cpp \
	hbqt_qapplication.cpp \
	hbqt_qboxlayout.cpp \
	hbqt_qbrush.cpp \
	hbqt_qcalendarwidget.cpp \
	hbqt_qcheckbox.cpp \
	hbqt_qcolor.cpp \
	hbqt_qcolordialog.cpp \
	hbqt_qcombobox.cpp \
	hbqt_qcommandlinkbutton.cpp \
	hbqt_qcoreapplication.cpp \
	hbqt_qdateedit.cpp \
	hbqt_qdatetimeedit.cpp \
	hbqt_qdial.cpp \
	hbqt_qdialog.cpp \
	hbqt_qdoublespinbox.cpp \
	hbqt_qerrormessage.cpp \
	hbqt_qfiledialog.cpp \
	hbqt_qfocusframe.cpp \
	hbqt_qfont.cpp \
	hbqt_qfontcombobox.cpp \
	hbqt_qfontdialog.cpp \
	hbqt_qformlayout.cpp \
	hbqt_qframe.cpp \
	hbqt_qgroupbox.cpp \
	hbqt_qhboxlayout.cpp \
	hbqt_qinputdialog.cpp \
	hbqt_qlabel.cpp \
	hbqt_qlayout.cpp \
	hbqt_qlayoutitem.cpp \
	hbqt_qlcdnumber.cpp \
	hbqt_qlineedit.cpp \
	hbqt_qlistview.cpp \
	hbqt_qmainwindow.cpp \
	hbqt_qmenu.cpp \
	hbqt_qmenubar.cpp \
	hbqt_qmessagebox.cpp \
	hbqt_qobject.cpp \
	hbqt_qpaintdevice.cpp \
	hbqt_qpagesetupdialog.cpp \
	hbqt_qpainter.cpp \
	hbqt_qpen.cpp \
	hbqt_qprintdialog.cpp \
	hbqt_qprintpreviewdialog.cpp \
	hbqt_qprogressbar.cpp \
	hbqt_qprogressdialog.cpp \
	hbqt_qpushbutton.cpp \
	hbqt_qradiobutton.cpp \
	hbqt_qscrollarea.cpp \
	hbqt_qscrollbar.cpp \
	hbqt_qsizegrip.cpp \
	hbqt_qslider.cpp \
	hbqt_qspinbox.cpp \
	hbqt_qsplitter.cpp \
	hbqt_qtabbar.cpp \
	hbqt_qtableview.cpp \
	hbqt_qtablewidget.cpp \
	hbqt_qtablewidgetitem.cpp \
	hbqt_qtabwidget.cpp \
	hbqt_qtextedit.cpp \
	hbqt_qtimeedit.cpp \
	hbqt_qtoolbar.cpp \
	hbqt_qtoolbox.cpp \
	hbqt_qtoolbutton.cpp \
	hbqt_qtreeview.cpp \
	hbqt_qtreewidget.cpp \
	hbqt_qtreewidgetitem.cpp \
	hbqt_qvboxlayout.cpp \
	hbqt_qwebpage.cpp \
	hbqt_qwidget.cpp \
	hbqt_qwizard.cpp \
	hbqt_qwebview.cpp \
	hbqt_base.cpp \
	hbqt_utils.cpp \
	hbqt_slots.cpp \
	moc_slots.cpp \


C_HEADERS=\
	hbqt.h \
	hbqt_slots.h \

#PRG_HEADERS=\
#   hbqt.ch \
#   hbqtextern.ch \

PRG_SOURCES=\
	TQAbstractButton.prg \
	TQAbstractItemView.prg \
	TQAbstractPrintDialog.prg \
	TQAbstractScrollArea.prg \
	TQAbstractSlider.prg \
	TQAbstractSpinBox.prg \
	TQAction.prg \
	TQBoxLayout.prg \
	TQCalendarWidget.prg \
	TQCheckBox.prg \
	TQColorDialog.prg \
	TQComboBox.prg \
	TQCommandLinkButton.prg \
	TQDateEdit.prg \
	TQDateTimeEdit.prg \
	TQDial.prg \
	TQDialog.prg \
	TQDoubleSpinBox.prg \
	TQErrorMessage.prg \
	TQFileDialog.prg \
	TQFocusFrame.prg \
	TQFontComboBox.prg \
	TQFontDialog.prg \
	TQFormLayout.prg \
	TQFrame.prg \
	TQGroupBox.prg \
	TQHBoxLayout.prg \
	TQInputDialog.prg \
	TQLabel.prg \
	TQLayout.prg \
	TQLayoutItem.prg \
	TQLCDNumber.prg \
	TQLineEdit.prg \
	TQListView.prg \
	TQMainWindow.prg \
	TQMenu.prg \
	TQMenuBar.prg \
	TQMessageBox.prg \
	TQObject.prg \
	TQPaintDevice.prg \
	TQPageSetupDialog.prg \
	TQPainter.prg \
	TQPrintDialog.prg \
	TQPrintPreviewDialog.prg \
	TQProgressBar.prg \
	TQProgressDialog.prg \
	TQPushButton.prg \
	TQRadioButton.prg \
	TQScrollArea.prg \
	TQScrollBar.prg \
	TQSizeGrip.prg \
	TQSlider.prg \
	TQSpinBox.prg \
	TQSplitter.prg \
	TQTabBar.prg \
	TQTableView.prg \
	TQTableWidget.prg \
	TQTableWidgetItem.prg \
	TQTabWidget.prg \
	TQTextEdit.prg \
	TQTimeEdit.prg \
	TQToolBar.prg \
	TQToolBox.prg \
	TQToolButton.prg \
	TQTreeView.prg \
	TQTreeWidget.prg \
	TQTreeWidgetItem.prg \
	TQVBoxLayout.prg \
	TQWebPage.prg \
	TQWidget.prg \
	TQWizard.prg \
	TQWebView.prg \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
else
include $(TOP)$(ROOT)config/none.cf
endif
