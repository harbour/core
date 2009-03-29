#
# $Id$
#

ROOT = ../../

LIBNAME=hbqt

ifneq ($(HB_ARCHITECTURE),dos)
ifneq ($(HB_ARCHITECTURE),os2)
ifneq ($(HB_COMPILER),mingw64)

ifeq ($(HB_INC_QT),)
ifeq ($(HB_XBUILD),)
HB_INC_QT = /usr/include/qt4 /Developer/qt/include
endif
endif

HB_INC_QT_OK += $(foreach d, $(HB_INC_QT), $(if $(wildcard $(d)/Qt/qglobal.h),$(d),))

ifneq ($(strip $(HB_INC_QT_OK)),)

HB_USER_CFLAGS += $(foreach d, $(HB_INC_QT_OK), -I$(d))

CPP_SOURCES=\
	hbqt_base.cpp \
	hbqt_qdialog.cpp \
	hbqt_qabstractbutton.cpp \
	hbqt_qabstractitemview.cpp \
	hbqt_qabstractprintdialog.cpp \
	hbqt_qwidget.cpp \
	hbqt_qabstractscrollarea.cpp \
	hbqt_qabstractslider.cpp \
	hbqt_qabstractspinbox.cpp \
	hbqt_qaction.cpp \
	hbqt_qapplication.cpp \
	hbqt_qboxlayout.cpp \
	hbqt_qcalendarwidget.cpp \
	hbqt_qcheckbox.cpp \
	hbqt_qcolordialog.cpp \
	hbqt_qcombobox.cpp \
	hbqt_qcommandlinkbutton.cpp \
	hbqt_qcoreapplication.cpp \
	hbqt_qdateedit.cpp \
	hbqt_qdatetimeedit.cpp \
	hbqt_qdial.cpp \
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
	hbqt_qpagesetupdialog.cpp \
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
	hbqt_qtextedit.cpp \
	hbqt_qtimeedit.cpp \
	hbqt_qtoolbar.cpp \
	hbqt_qtoolbox.cpp \
	hbqt_qtoolbutton.cpp \
	hbqt_qtreeview.cpp \
	hbqt_qvboxlayout.cpp \
	hbqt_qwebview.cpp \
	hbqt_qwizard.cpp \
	hbqt_slots.cpp \
	moc_slots.cpp \
	hbqt_utils.cpp \
	hbqt_qtreewidget.cpp \
	hbqt_qtreewidgetitem.cpp \


C_HEADERS=\
	hbqt.h \
	slots.h \

#PRG_HEADERS=\
#	hbqt.ch \
#	hbqtextern.ch \

#PRG_SOURCES=\
#	xxx.prg \

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
else
include $(TOP)$(ROOT)config/none.cf
endif
else
include $(TOP)$(ROOT)config/none.cf
endif
